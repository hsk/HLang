(* 型付きの構文木を、継続だけの中間表現に変換する。

   変換する関数は「この先の続きを作る関数」k を受け取る。式なら「値を受け取って続きを作る」、
   文なら「変数の対応表を受け取って続きを作る」。
   代入は対応表を書き換えるだけ。合流点では、書き換わった変数が継続の引数になる。 *)

open Ir
module A = Ast

type env = (string * (atom * ty)) list           (* 変数 -> (いまの値, 型) *)

type loop = { brk : string; cont : string; vars : string list }

let ty_of t = if A.wide t then U16 else U8

let count = ref 0
let fresh base =
  incr count;
  Printf.sprintf "%s.%d" base !count

let strs : (string, string) Hashtbl.t = Hashtbl.create 16    (* 文字列 -> ラベル *)
let str s =
  match Hashtbl.find_opt strs s with
  | Some l -> l
  | None ->
    let l = Printf.sprintf "str.%d" (Hashtbl.length strs) in
    Hashtbl.replace strs s l;
    l

let bind t e k = let x = fresh "t" in Let (x, t, e, k (V x))

(* 256 バイト以下の表。ページをまたがないように置くので、番地計算は下位バイトの足し算で済む。 *)
let small : (string, unit) Hashtbl.t = Hashtbl.create 16

let pools : (string, unit) Hashtbl.t = Hashtbl.create 8       (* プールのタスク *)
let frames : (string * int * int) list ref = ref []           (* プールのタスク: 名前, 個数, フレームの大きさ *)

(* 計算し直しても同じで、何も起こさない番地の式か（変数、変数 + 定数） *)
let rec plain (e : A.ty A.expr) =
  match e.x with
  | Var _ | Addr _ | Int _ -> true
  | Bin (("+" | "-"), l, r) -> plain l && plain r
  | _ -> false

(* 符号付きの大小は、符号ビットを反転してから符号なしで比べれば同じ順になる *)
let flip (ty : A.ty) t o a k =
  let s = if t = U8 then 0x80 else 0x8000 in
  if not (A.signed ty) || o = "==" || o = "!=" then k a
  else match a with
    | C n -> k (C (n lxor s))
    | a -> let x = fresh "t" in Let (x, t, Bin ("^", a, C s), k (V x))

let is_cmp o = List.mem o [ "=="; "!="; "<"; "<="; ">"; ">=" ]

(* 式: 値を k に渡す *)
let rec exp (env : env) (e : A.ty A.expr) (k : atom -> term) : term =
  let t = ty_of e.a in
  match e.x with
  | Int n -> k (C n)
  | Tf b -> k (C (Bool.to_int b))
  | Str s -> k (S (str s, 0))
  | Var v -> k (fst (List.assoc v env))
  | Glob g -> bind t (Load (S (g, 0))) k
  | Addr x -> k (S (x, 0))
  | Index _ | Field _ -> assert false
  | Lut (f, a) -> exp env a (fun a -> bind t (Lut (f ^ ".lut", a)) k)
  | Bin (("&&" | "||"), _, _) | Un ("!", _) | Running _ ->
    (* 真偽を値にする: 合流する継続へ 1 か 0 を渡す *)
    let j = fresh "bool" and x = fresh "t" in
    LetK ([ { k = j; ps = [ x, U8 ]; body = k (V x) } ],
          cond env e (fun () -> Jump (j, [ C 1 ])) (fun () -> Jump (j, [ C 0 ])))
  | Bin (o, l, r) when is_cmp o ->
    let t = ty_of l.a in
    exp env l (fun a -> exp env r (fun b -> flip l.a t o a (fun a -> flip l.a t o b (fun b -> bind U8 (Cmp (o, t, a, b)) k))))
  | Bin ("+", { x = Addr s; _ }, { x = Cast (A.U16, i); _ }) when Hashtbl.mem small s && not (A.wide i.a) ->
    exp env i (fun a -> bind U16 (Idx (s, a)) k)
  | Bin ("+", { x = Addr s; _ }, { x = Bin ("<<", { x = Cast (A.U16, i); _ }, { x = Int 1; _ }); _ })
    when Hashtbl.mem small s && not (A.wide i.a) ->
    exp env i (fun a -> bind U8 (Sh ("<<", a, 1)) (fun d -> bind U16 (Idx (s, d)) k))
  | Bin ((("<<" | ">>") as o), l, { x = Int n; _ }) ->
    let o = if o = ">>" && A.signed l.a then "sar" else o in      (* 符号付きの >> は、符号ビットを保つ *)
    exp env l (fun a -> bind t (Sh (o, a, n)) k)
  | Bin ("*", l, { x = Int n; _ }) -> exp env l (fun a -> bind t (Mulc (a, n)) k)
  | Bin ((("/" | "%") as o), l, r) ->
    (* 割り算と余りは、実行時のルーチン（Alloc の divmod）を呼ぶ *)
    let f = (if A.signed e.a then "s" else "") ^ (if o = "/" then "div" else "mod") ^ if t = U8 then "8" else "16" in
    exp env l (fun a -> exp env r (fun b -> let x = fresh "t" in Call (Some (x, t), "." ^ f, [ a, t; b, t ], k (V x))))
  | Bin (o, l, r) -> exp env l (fun a -> exp env r (fun b -> bind t (Bin (o, a, b)) k))
  | Un (o, a) -> exp env a (fun a -> bind t (Un (o, a)) k)
  | Call (f, args) ->
    let rec go acc = function
      | [] ->
        let args = List.rev acc in
        if e.a = A.Unit then Call (None, f, args, k (C 0))
        else (let x = fresh "t" in Call (Some (x, t), f, args, k (V x)))
      | (a : A.ty A.expr) :: rest -> exp env a (fun v -> go ((v, ty_of a.a) :: acc) rest)
    in
    go [] args
  | Mem (_, a) -> exp env a (fun a -> bind t (Load a) k)
  | Spawn (t, args) when Hashtbl.mem pools t ->
    (* 空いているフレームをもらい（HL で返る）、その先頭から順に引数を書く。フレームの番地が式の値 *)
    let rec go acc = function
      | (a : A.ty A.expr) :: rest -> exp env a (fun v -> go ((v, ty_of a.a) :: acc) rest)
      | [] ->
        let s = fresh "t" in
        let rec store off = function
          | [] -> k (V s)
          | (v, t) :: rest ->
            let next = store (off + if t = U8 then 1 else 2) rest in
            if off = 0 then Store (t, V s, v, next)
            else (let m = fresh "t" in Let (m, U16, Bin ("+", V s, C off), Store (t, V m, v, next)))
        in
        Call (Some (s, U16), t ^ ".spawn", [], store 0 (List.rev acc))
    in
    go [] args
  | Spawn (t, _) -> Store (U16, S (t ^ ".pc", 0), S (t, 0), k (C 0))  (* 続きの番地を、本体の先頭にする *)
  | Port { x = Int p; _ } -> bind U8 (In p) k
  | Port _ -> assert false
  | Cast (to_, a) ->
    (* 幅で考える: 真偽は 0/1 の 1 バイト、列挙型も 1 バイト *)
    exp env a (fun v ->
      if to_ = A.Bool then bind U8 (Cmp ("!=", ty_of a.a, v, C 0)) k
      else if A.wide a.a = A.wide to_ then k v
      else if A.wide to_ && a.a = A.I8 then
        (* 符号を保って広げる: 符号ビットを反転して広げ、0x80 を引く *)
        bind U8 (Bin ("^", v, C 0x80)) (fun x -> bind U16 (Zext x) (fun y -> bind U16 (Bin ("-", y, C 0x80)) k))
      else if A.wide to_ then bind U16 (Zext v) k
      else bind U8 (Trunc v) k)

(* 条件: 成り立てば yes、成り立たなければ no の作る続きへ。
   && と || は yes/no を 2 回使うことがあるので、yes/no は小さな飛び先であること。 *)
and cond env (e : A.ty A.expr) yes no : term =
  match e.x with
  | Tf true -> yes ()
  | Tf false -> no ()
  | Bin ("&&", l, r) -> cond env l (fun () -> cond env r yes no) no
  | Bin ("||", l, r) -> cond env l yes (fun () -> cond env r yes no)
  | Un ("!", a) -> cond env a no yes
  | Running t when Hashtbl.mem pools t ->
    let x = fresh "t" in
    Call (Some (x, U8), t ^ ".live", [], If ("!=", U8, V x, C 0, yes (), no ()))
  | Running t ->
    (* 続きの番地が「終わり」を指していなければ、まだ動いている *)
    bind U16 (Load (S (t ^ ".pc", 0))) (fun a -> If ("!=", U16, a, S (t ^ ".done", 0), yes (), no ()))
  | Bin (o, l, r) when is_cmp o ->
    let t = ty_of l.a in
    exp env l (fun a -> exp env r (fun b -> flip l.a t o a (fun a -> flip l.a t o b (fun b -> If (o, t, a, b, yes (), no ())))))
  | _ -> exp env e (fun a -> If ("!=", U8, a, C 0, yes (), no ()))

let rec simple (e : A.ty A.expr) =                (* yes/no を 1 回ずつしか使わない条件か *)
  match e.x with
  | Bin (("&&" | "||"), _, _) -> false
  | Un ("!", a) -> simple a
  | _ -> true

(* 条件分岐。単純な条件なら枝をそのまま埋め込み、複雑なら枝を継続にして飛ぶ。 *)
let branch env c yes no =
  if simple c then cond env c yes no
  else begin
    let y = fresh "then" and n = fresh "else" in
    LetK ([ { k = y; ps = []; body = yes () }; { k = n; ps = []; body = no () } ],
          cond env c (fun () -> Jump (y, [])) (fun () -> Jump (n, [])))
  end

(* 文の並びの中で代入される変数 *)
let rec assigned ss =
  List.concat_map
    (function
      | A.Set ({ x = Var x; _ }, _) -> [ x ]
      | If (_, t, f) -> assigned t @ assigned f
      | Match (_, arms, other) ->
        List.concat_map (fun (_, b) -> assigned b) arms @ assigned (Option.value ~default:[] other)
      | While (_, b) | For (_, _, _, b) | Each (_, _, b) -> assigned b
      | _ -> [])
    ss

(* 合流点を作る準備: 書き換わる変数ごとに、継続の引数を用意する *)
let merge (env : env) ss =
  let vars = List.sort_uniq compare (List.filter (fun x -> List.mem_assoc x env) (assigned ss)) in
  let ps () = List.map (fun x -> fresh x, snd (List.assoc x env)) vars in
  let enter ps = List.fold_left2 (fun env x (p, t) -> (x, (V p, t)) :: List.remove_assoc x env) env vars ps in
  vars, ps, enter

let pass (env : env) vars = List.map (fun x -> fst (List.assoc x env)) vars

let rec block lp env ss (k : env -> term) : term =
  match ss with
  | [] -> k env
  | s :: rest -> stmt lp env s (fun env -> block lp env rest k)

and stmt lp (env : env) (s : A.ty A.stmt) (k : env -> term) : term =
  match s with
  | Let (x, _, e) -> exp env e (fun a -> k ((x, (a, ty_of e.a)) :: env))
  | Set ({ x = Var x; _ }, e) ->
    exp env e (fun a -> k ((x, (a, ty_of e.a)) :: List.remove_assoc x env))
  | Set ({ x = Glob g; _ }, e) -> exp env e (fun a -> Store (ty_of e.a, S (g, 0), a, k env))
  | Set ({ x = Mem (t, p); _ }, e) when plain p ->
    (* 番地の計算を書き込みの直前に置く。コード生成が 1 つの命令にまとめやすい *)
    exp env e (fun a -> exp env p (fun p -> Store (ty_of t, p, a, k env)))
  | Set ({ x = Mem (t, p); _ }, e) ->
    exp env p (fun p -> exp env e (fun a -> Store (ty_of t, p, a, k env)))
  | Set ({ x = Port { x = Int p; _ }; _ }, e) -> exp env e (fun a -> Out (p, a, k env))
  | Set _ -> assert false
  | Do e -> exp env e (fun _ -> k env)
  | Return None -> Ret None
  | Return (Some e) -> exp env e (fun a -> Ret (Some (a, ty_of e.a)))
  | Yield -> Yield (k env)
  | Wait _ -> assert false
  | Resume t -> Call (None, t ^ ".resume", [], k env)
  | Each (x, t, b) -> block lp env (Check.each !frames x t b) (fun env -> k (List.remove_assoc x (List.remove_assoc (x ^ ".left") env)))
  | Stop (t, None) when Hashtbl.mem pools t ->
    stmt lp env (Each ("stop." ^ t, t, [ Stop (t, Some { x = Var ("stop." ^ t); a = A.Ptr t }) ])) k
  | Stop (t, None) -> Store (U16, S (t ^ ".pc", 0), S (t ^ ".done", 0), k env)
  | Stop (_, Some e) ->
    (* 続きの番地の上位バイトを 0 にすれば、その枠は空きになる *)
    exp env e (fun a -> let m = fresh "t" in Let (m, U16, Bin ("-", a, C 1), Store (U8, V m, C 0, k env)))
  | Break -> let l = Option.get lp in Jump (l.brk, pass env l.vars)
  | Continue -> let l = Option.get lp in Jump (l.cont, pass env l.vars)
  | If (c, t, f) ->
    let vars, ps, enter = merge env (t @ f) in
    let j = fresh "join" and ps = ps () in
    let go ss () = block lp env ss (fun env -> Jump (j, pass env vars)) in
    LetK ([ { k = j; ps; body = k (enter ps) } ], branch env c (go t) (go f))
  | Match (e, arms, other) ->
    (* 枝の本体は引数なしの継続にして、値ごとの飛び先にする。
       順に比べるか表で飛ぶかは、コード生成が決める。 *)
    let bodies = List.concat_map snd arms @ Option.value ~default:[] other in
    let vars, ps, enter = merge env bodies in
    let j = fresh "join" and ps = ps () in
    let arm b = { k = fresh "case"; ps = []; body = block lp env b (fun env -> Jump (j, pass env vars)) } in
    let arms = List.map (fun (vs, b) -> List.map (fun (v : A.ty A.expr) -> match v.x with Int n -> n | _ -> assert false) vs, arm b) arms in
    let other = Option.map arm other in
    let tests = List.concat_map (fun (vs, (kont : kont)) -> List.map (fun n -> n, kont.k) vs) arms in
    exp env e (fun a ->
      let konts = List.map snd arms @ Option.to_list other in
      LetK ([ { k = j; ps; body = k (enter ps) } ],
            if konts = [] then Jump (j, pass env vars)
            else LetK (konts, Switch (a, tests, Option.map (fun (o : kont) -> o.k) other))))
  | While (c, b) ->
    let vars, ps, enter = merge env b in
    let top = fresh "loop" and out = fresh "done" in
    let ps_top = ps () and ps_out = ps () in
    let env' = enter ps_top in
    let lp' = Some { brk = out; cont = top; vars } in
    LetK ([ { k = out; ps = ps_out; body = k (enter ps_out) } ],
      LetK ([ { k = top; ps = ps_top;
                body = branch env' c
                    (fun () -> block lp' env' b (fun env -> Jump (top, pass env vars)))
                    (fun () -> Jump (out, pass env' vars)) } ],
        Jump (top, pass env vars)))
  | For (x, lo, hi, b) ->
    (* x も「書き換わる変数」の 1 つとして回す。continue は増分をする継続 next へ。 *)
    let t = ty_of lo.a in
    exp env lo (fun lo ->
      exp env hi (fun hi ->
        let env = (x, (lo, t)) :: env in
        let vars, ps, enter = merge env (A.Set ({ x = Var x; a = A.U8 }, { x = Int 0; a = A.U8 }) :: b) in
        let top = fresh "loop" and out = fresh "done" and next = fresh "next" in
        let ps_top = ps () and ps_out = ps () and ps_next = ps () in
        let env' = enter ps_top in
        let lp' = Some { brk = out; cont = next; vars } in
        let step =
          let e = enter ps_next in
          let i = fresh x in
          Let (i, t, Bin ("+", fst (List.assoc x e), C 1),
               Jump (top, pass ((x, (V i, t)) :: List.remove_assoc x e) vars))
        in
        LetK ([ { k = out; ps = ps_out; body = k (List.remove_assoc x (enter ps_out)) } ],
          LetK ([ { k = top; ps = ps_top;
                    body = If ("<", t, fst (List.assoc x env'), hi,
                      LetK ([ { k = next; ps = ps_next; body = step } ],
                        block lp' env' b (fun env -> Jump (next, pass env vars))),
                      Jump (out, pass env' vars)) } ],
            Jump (top, pass env vars)))))

let fn (f : A.ty A.fn) : fn =
  (* プールのタスクの引数はフレームにある。関数としての引数は、フレームを指す self だけ *)
  let src = if f.pool > 0 then [ f.self, A.U16 ] else f.ps in
  let ps = List.map (fun (x, t) -> fresh x, ty_of t) src in
  let env = List.map2 (fun (x, _) (p, t) -> x, (V p, t)) src ps in
  { name = f.name; ps; ret = (if f.ret = A.Unit then None else Some (ty_of f.ret));
    body = block None env f.body (fun _ -> Ret None); task = f.task;
    self = (if f.pool > 0 then Some (fst (List.hd ps)) else None) }

let convert (p : Check.prog) : fn list =
  count := 0;
  Hashtbl.reset strs;
  Hashtbl.reset small;
  Hashtbl.reset pools;
  List.iter (fun (t, _, _) -> Hashtbl.replace pools t ()) p.pools;
  frames := p.pools;
  let w t = if A.wide t then 2 else 1 in
  List.iter (fun (x, t, n) -> if n * w t <= 256 then Hashtbl.replace small x ()) p.arrays;
  List.iter (fun (x, t, vs) -> if List.length vs * w t <= 256 then Hashtbl.replace small x ()) p.datas;
  List.map fn p.fns
