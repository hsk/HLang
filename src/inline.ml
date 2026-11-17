(* インライン展開: 小さな関数の呼び出しを、その本体で置き換える。

   call と ret（27T）と、引数の受け渡しが消える。呼ぶ側では、呼び出しをまたいで値をレジスタに
   持ち越せるようになり（相手が何を壊すかが見える）、展開した本体も、呼ぶ側の値と一緒に最適化される。

     let x = f(a, b); k    ->   LetK ([ret(x) = k], let p = a; let q = b; <f の本体>)
     f の本体の return v   ->   ret(v)

   展開する本体の変数と継続は、名前を付け替える（関数の中で名前は 1 つずつ）。
   対象: タスクでなく、main でなく、yield を含まない関数で、inline fn と書いたもの。とても小さい関数
   （項が 8 個まで）は、書かなくても展開する。どこまで展開するかは、コードの大きさとの兼ね合いなので、
   大きい関数は書く人が決める。番地を取られている関数（&f）は、展開しても残る。 *)

open Ir

let tiny = 8            (* inline と書かなくても展開する大きさ（項の数）。damaxian で測って決めた *)

let rec size = function
  | Let (_, _, _, k) | Store (_, _, _, k) | Out (_, _, k) | Call (_, _, _, k) | Yield k -> 1 + size k
  | LetK (ks, b) -> List.fold_left (fun n (k : kont) -> n + size k.body) (size b) ks
  | If (_, _, _, _, y, n) -> 1 + size y + size n
  | Jump _ | Switch _ | Ret _ -> 1

let rec has_yield = function
  | Yield _ -> true
  | Let (_, _, _, k) | Store (_, _, _, k) | Out (_, _, k) | Call (_, _, _, k) -> has_yield k
  | LetK (ks, b) -> has_yield b || List.exists (fun (k : kont) -> has_yield k.body) ks
  | If (_, _, _, _, y, n) -> has_yield y || has_yield n
  | Jump _ | Switch _ | Ret _ -> false

let rec calls acc = function
  | Call (_, g, _, k) -> calls (g :: acc) k
  | Let (_, _, _, k) | Store (_, _, _, k) | Out (_, _, k) | Yield k -> calls acc k
  | LetK (ks, b) -> List.fold_left (fun acc (k : kont) -> calls acc k.body) (calls acc b) ks
  | If (_, _, _, _, y, n) -> calls (calls acc y) n
  | Jump _ | Switch _ | Ret _ -> acc

let count = ref 0

(* f の本体を、名前を付け替えて、ret へ飛ぶ形にする *)
let copy (f : fn) args r k =
  incr count;
  let sfx = Printf.sprintf ".i%d" !count in
  let names = Hashtbl.create 32 in
  let bind x = let y = x ^ sfx in Hashtbl.replace names x y; y in
  let nm x = Option.value ~default:x (Hashtbl.find_opt names x) in
  let at = function V x -> V (nm x) | a -> a in
  let ex = function
    | Atom a -> Atom (at a) | Bin (o, a, b) -> Bin (o, at a, at b) | Sh (o, a, n) -> Sh (o, at a, n)
    | Mulc (a, n) -> Mulc (at a, n) | Idx (s, a) -> Idx (s, at a) | Lut (s, a) -> Lut (s, at a)
    | Un (o, a) -> Un (o, at a) | Zext a -> Zext (at a) | Trunc a -> Trunc (at a)
    | Cmp (o, t, a, b) -> Cmp (o, t, at a, at b) | Load a -> Load (at a) | In p -> In p
  in
  let ret = "ret" ^ sfx in
  let rec go = function
    (* 名前を付け替えてから続きを処理する（OCaml は引数を右から評価するので、let で順を決める） *)
    | Let (x, ty, e, k) -> let e = ex e in let x = bind x in Let (x, ty, e, go k)
    | Store (ty, a, v, k) -> Store (ty, at a, at v, go k)
    | Out (p, v, k) -> Out (p, at v, go k)
    | Call (r, g, args, k) ->
      let args = List.map (fun (a, t) -> at a, t) args in
      let r = Option.map (fun (x, t) -> bind x, t) r in
      Call (r, g, args, go k)
    | LetK (ks, b) ->
      List.iter (fun (k : kont) -> ignore (bind k.k)) ks;
      let ks = List.map (fun (k : kont) -> let ps = List.map (fun (p, t) -> bind p, t) k.ps in { k = nm k.k; ps; body = go k.body }) ks in
      LetK (ks, go b)
    | Jump (k, args) -> Jump (nm k, List.map at args)
    | If (o, ty, a, b, y, n) -> If (o, ty, at a, at b, go y, go n)
    | Switch (a, cases, other) -> Switch (at a, List.map (fun (v, k) -> v, nm k) cases, Option.map nm other)
    | Ret None -> Jump (ret, if r = None then [] else [ C 0 ])   (* 値を返す関数の、届かない終わり（match の後など） *)
    | Ret (Some (a, _)) -> Jump (ret, if r = None then [] else [ at a ])
    | Yield k -> Yield (go k)
  in
  let ps = List.map (fun (p, t) -> bind p, t) f.ps in
  let body = go f.body in
  let body = List.fold_right2 (fun (p, t) (a, _) b -> Let (p, t, Atom a, b)) ps args body in
  LetK ([ { k = ret; ps = Option.to_list r; body = k } ], body)

let program (fns : fn list) : fn list =
  let tbl = Hashtbl.create 64 in
  List.iter (fun (f : fn) -> Hashtbl.replace tbl f.name f) fns;
  let sites = Hashtbl.create 64 in
  List.iter
    (fun (f : fn) -> List.iter (fun g -> Hashtbl.replace sites g (1 + Option.value ~default:0 (Hashtbl.find_opt sites g))) (calls [] f.body))
    fns;
  let ok (g : fn) =
    (not g.task) && g.self = None && g.name <> "main" && (not (has_yield g.body))
    && not (List.mem g.name (calls [] g.body))
    && (g.inline || size g.body <= tiny)
  in
  (* 呼ばれる側を先に展開しておく（展開した本体の中の呼び出しも、展開済みになる）。深さは 3 まで *)
  let rec expand depth t =
    let ex = expand depth in
    match t with
    | Call (r, g, args, k) when depth > 0 && (match Hashtbl.find_opt tbl g with Some g -> ok g | None -> false) ->
      let g = Hashtbl.find tbl g in
      copy { g with body = expand (depth - 1) g.body } args r (ex k)
    | Call (r, g, args, k) -> Call (r, g, args, ex k)
    | Let (x, ty, e, k) -> Let (x, ty, e, ex k)
    | Store (ty, a, v, k) -> Store (ty, a, v, ex k)
    | Out (p, v, k) -> Out (p, v, ex k)
    | LetK (ks, b) -> LetK (List.map (fun (k : kont) -> { k with body = ex k.body }) ks, ex b)
    | If (o, ty, a, b, y, n) -> If (o, ty, a, b, ex y, ex n)
    | Yield k -> Yield (ex k)
    | (Jump _ | Switch _ | Ret _) as t -> t
  in
  List.map (fun (f : fn) -> { f with body = expand 3 f.body }) fns
