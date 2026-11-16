(* 生存解析。中間表現の各地点に「ここから先で使われる変数」の集合 lv を付ける。

   継続の中から外の変数を使えるので、継続へ飛ぶ地点では「その継続が使う外の変数」も生きている。
   継続どうしは呼び合うので、集合が変わらなくなるまで繰り返して求める。 *)

open Ir
module SS = Set.Make (String)

type t = { n : node; lv : SS.t }

and node =
  | Let of string * ty * exp * t
  | Store of ty * atom * atom * t
  | Out of int * atom * t
  | Call of (string * ty) option * string * (atom * ty) list * t
  | LetK of kont list * t
  | Jump of string * atom list
  | If of string * ty * atom * atom * t * t
  | Switch of atom * (int * string) list * string option
  | Ret of (atom * ty) option
  | Yield of t

and kont = {
  k : string;
  ps : (string * ty) list;
  body : t;
  loop : bool;                   (* 自分へ戻ってくる（ループの先頭） *)
  used : bool;                   (* どこかから飛んでくる *)
}

let of_atoms l = List.fold_left (fun s a -> match a with V x -> SS.add x s | _ -> s) SS.empty l

let atoms_of = function
  | Atom a | Sh (_, a, _) | Mulc (a, _) | Idx (_, a) | Lut (_, a) | Un (_, a) | Zext a | Trunc a | Load a -> [ a ]
  | Bin (_, a, b) | Cmp (_, _, a, b) -> [ a; b ]
  | In _ -> []

(* 項の中にある飛び先の名前 *)
let rec targets = function
  | Ir.Let (_, _, _, k) | Ir.Store (_, _, _, k) | Ir.Out (_, _, k) | Ir.Call (_, _, _, k) | Ir.Yield k -> targets k
  | Ir.LetK (ks, b) -> List.fold_left (fun s (k : Ir.kont) -> SS.union s (targets k.body)) (targets b) ks
  | Ir.Jump (k, _) -> SS.singleton k
  | Ir.If (_, _, _, _, a, b) -> SS.union (targets a) (targets b)
  | Ir.Switch (_, cases, other) -> SS.of_list (List.map snd cases @ Option.to_list other)
  | Ir.Ret _ -> SS.empty

let annotate (f : Ir.fn) : t =
  let kfv = Hashtbl.create 16 in                       (* 継続 -> 外から持ち込む変数 *)
  let all = targets f.body in
  let changed = ref true in
  let rec go (t : Ir.term) : t =
    match t with
    | Ir.Let (x, ty, e, k) ->
      let k = go k in
      { n = Let (x, ty, e, k); lv = SS.union (of_atoms (atoms_of e)) (SS.remove x k.lv) }
    | Ir.Store (ty, a, v, k) ->
      let k = go k in
      { n = Store (ty, a, v, k); lv = SS.union (of_atoms [ a; v ]) k.lv }
    | Ir.Out (p, v, k) ->
      let k = go k in
      { n = Out (p, v, k); lv = SS.union (of_atoms [ v ]) k.lv }
    | Ir.Call (r, g, args, k) ->
      let k = go k in
      let after = match r with Some (x, _) -> SS.remove x k.lv | None -> k.lv in
      { n = Call (r, g, args, k); lv = SS.union (of_atoms (List.map fst args)) after }
    | Ir.LetK (ks, body) ->
      let ks =
        List.map
          (fun (k : Ir.kont) ->
            let body = go k.body in
            let fv = List.fold_left (fun s (p, _) -> SS.remove p s) body.lv k.ps in
            if not (SS.equal fv (Option.value ~default:SS.empty (Hashtbl.find_opt kfv k.k))) then begin
              Hashtbl.replace kfv k.k fv;
              changed := true
            end;
            { k = k.k; ps = k.ps; body; loop = SS.mem k.k (targets k.body); used = SS.mem k.k all })
          ks
      in
      let body = go body in
      { n = LetK (ks, body); lv = body.lv }
    | Ir.Jump (k, args) ->
      { n = Jump (k, args);
        lv = SS.union (of_atoms args) (Option.value ~default:SS.empty (Hashtbl.find_opt kfv k)) }
    | Ir.If (o, ty, a, b, y, n) ->
      let y = go y in
      let n = go n in
      { n = If (o, ty, a, b, y, n); lv = SS.union (of_atoms [ a; b ]) (SS.union y.lv n.lv) }
    | Ir.Switch (a, cases, other) ->
      let fv k = Option.value ~default:SS.empty (Hashtbl.find_opt kfv k) in
      { n = Switch (a, cases, other);
        lv = List.fold_left (fun s k -> SS.union s (fv k)) (of_atoms [ a ]) (List.map snd cases @ Option.to_list other) }
    | Ir.Ret r ->
      { n = Ret r; lv = (match r with Some (a, _) -> of_atoms [ a ] | None -> SS.empty) }
    | Ir.Yield k -> let k = go k in { n = Yield k; lv = k.lv }
  in
  let r = ref (go f.body) in
  while !changed do
    changed := false;
    r := go f.body
  done;
  (* プールのタスクでは、フレームを指す self がどこでも生きている（中断や終了のときに要る） *)
  let rec always s (t : t) =
    let k = always s in
    let n =
      match t.n with
      | Let (x, ty, e, b) -> Let (x, ty, e, k b)
      | Store (ty, a, v, b) -> Store (ty, a, v, k b)
      | Out (p, v, b) -> Out (p, v, k b)
      | Call (r, g, args, b) -> Call (r, g, args, k b)
      | LetK (ks, b) -> LetK (List.map (fun (c : kont) -> { c with body = k c.body }) ks, k b)
      | If (o, ty, a, b, y, n) -> If (o, ty, a, b, k y, k n)
      | Yield b -> Yield (k b)
      | (Jump _ | Switch _ | Ret _) as n -> n
    in
    { n; lv = SS.add s t.lv }
  in
  match f.self with Some s -> always s !r | None -> !r

(* 関数に出てくる変数を、型と重み（ループの深さを考えた使用回数）つきで。重い順。 *)
let vars (f : Ir.fn) : (string * ty * int) list =
  let tbl = Hashtbl.create 64 and order = ref [] in
  let def x t = if not (Hashtbl.mem tbl x) then (Hashtbl.replace tbl x (t, 0); order := x :: !order) in
  let use w a =
    match a with
    | V x -> (match Hashtbl.find_opt tbl x with Some (t, n) -> Hashtbl.replace tbl x (t, n + w) | None -> ())
    | _ -> ()
  in
  List.iter (fun (x, t) -> def x t) f.ps;
  let rec go w = function
    | Ir.Let (x, t, e, k) -> List.iter (use w) (atoms_of e); def x t; use w (V x); go w k
    | Ir.Store (_, a, v, k) -> use w a; use w v; go w k
    | Ir.Out (_, v, k) -> use w v; go w k
    | Ir.Yield k -> go w k
    | Ir.Call (r, _, args, k) ->
      List.iter (fun (a, _) -> use w a) args;
      (match r with Some (x, t) -> def x t | None -> ());
      go w k
    | Ir.LetK (ks, b) ->
      List.iter (fun (k : Ir.kont) -> List.iter (fun (x, t) -> def x t) k.ps) ks;
      go w b;
      List.iter (fun (k : Ir.kont) -> go (if SS.mem k.k (targets k.body) then w * 8 else w) k.body) ks
    | Ir.Jump (_, args) -> List.iter (use w) args
    | Ir.If (_, _, a, b, y, n) -> use w a; use w b; go w y; go w n
    | Ir.Switch (a, _, _) -> use w a
    | Ir.Ret (Some (a, _)) -> use w a
    | Ir.Ret None -> ()
  in
  go 1 f.body;
  let l = List.rev_map (fun x -> let t, n = Hashtbl.find tbl x in x, t, n) !order in
  List.stable_sort (fun (_, _, a) (_, _, b) -> compare b a) l

(* 「同じ置き場所にすると得をしそうな変数の組」。
   継続へ渡す値とその引数、演算の結果とその左の値（その場で書き換えられる）。 *)
let pairs (f : Ir.fn) : (string * string) list =
  let ps = Hashtbl.create 16 and out = ref [] in
  let rec scan = function
    | Ir.LetK (ks, b) -> List.iter (fun (k : Ir.kont) -> Hashtbl.replace ps k.k k.ps; scan k.body) ks; scan b
    | Ir.Let (_, _, _, k) | Ir.Store (_, _, _, k) | Ir.Out (_, _, k) | Ir.Call (_, _, _, k) | Ir.Yield k -> scan k
    | Ir.If (_, _, _, _, a, b) -> scan a; scan b
    | _ -> ()
  in
  scan f.body;
  let rec go = function
    | Ir.Let (x, _, e, k) ->
      (match e with
       | Atom (V a) | Bin (_, V a, _) | Sh (_, V a, _) | Un (_, V a) -> out := (x, a) :: !out
       | _ -> ());
      go k
    | Ir.Store (_, _, _, k) | Ir.Out (_, _, k) | Ir.Call (_, _, _, k) | Ir.Yield k -> go k
    | Ir.LetK (ks, b) -> go b; List.iter (fun (k : Ir.kont) -> go k.body) ks
    | Ir.Jump (k, args) ->
      List.iter2 (fun (p, _) a -> match a with V a -> out := (p, a) :: !out | _ -> ()) (Hashtbl.find ps k) args
    | Ir.If (_, _, _, _, a, b) -> go a; go b
    | Ir.Switch _ | Ir.Ret _ -> ()
  in
  go f.body;
  List.rev !out
