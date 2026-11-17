(* ブロック転送: バイトのコピーと塗りつぶしのループを、ldir / lddr にする。

     for i in 0..n { dst[i] = src[i] }                    ->  ldir
     var i = n  while i != 0 { i -= 1  dst[i] = src[i] }  ->  lddr（終わりの番地から後ろへ）
     どちらの向きでも dst[i] = v                           ->  .fill（ld (hl),a のあと ldir）

   ldir は 1 バイト 21T。ループは 1 バイト 60T 以上かかる。形が合うものだけ置き換える。
   向きはループのとおりにする。ldir は前から、lddr は後ろから 1 バイトずつ写すので、番地が重なるときも、
   ループと同じ結果になる（重なる写しは、向きを間違えると壊れる）。塗りつぶしは、順序が結果に響かないので、
   どちらの向きでも前から埋める。
   ループの形（Cps が作る）: 上へ: loop(i) = if i < n then <本体>; next(i) else done(i)、next(i) = loop(i + 1)。
   下へ: loop(i) = if i != 0 then let t = i - 1; <本体 t>; loop(t) else done(i)。
   本体は、番地の計算（dst + i、src + i）、1 バイトの読み、1 バイトの書きだけ。
   ループを抜けた後の i は、上へなら n（n が 0 でも）、下へなら 0。 *)

open Ir

let count = ref 0
let fresh () = incr count; Printf.sprintf "z.b%d" !count

type kind = Z | A of atom | B of atom | P of int | Inv      (* i の 16bit 化 | 番地 base + i | base + i から読んだ値 | ポートから読んだ値 | ループによらない値 *)

(* 本体を調べる。i は添字の変数、fin は本体の最後（次へ進む飛び先）。
   コピーなら (dst, `Copy src)、塗りつぶしなら (dst, `Fill 値)、ポートへ書くなら (src, `Out ポート)、
   ポートから読むなら (dst, `In ポート)。あわせて、本体の中にあった「ループによらない計算」
   （p + 1 など）を返す。ループがなくなるので、呼ぶ前に出し直す。 *)
let body i (t : term) fin =
  let env = Hashtbl.create 8 in
  let hoist = ref [] in
  let bound a = match a with V x -> x = i || (match Hashtbl.find_opt env x with Some Inv | None -> false | Some _ -> true) | _ -> false in
  let inv a = not (bound a) in
  let pure = function
    | Atom p | Zext p | Trunc p | Un (_, p) | Sh (_, p, _) | Mulc (p, _) -> inv p
    | Bin (_, p, q) -> inv p && inv q
    | _ -> false
  in
  let rec scan = function
    | Let (x, U16, Zext (V v), k) when v = i -> Hashtbl.replace env x Z; scan k
    | Let (x, U16, Idx (s, V v), k) when v = i -> Hashtbl.replace env x (A (S (s, 0))); scan k
    | Let (x, U16, Bin ("+", a, V z), k) when Hashtbl.find_opt env z = Some Z && inv a ->
      Hashtbl.replace env x (A a); scan k
    | Let (x, U16, Bin ("+", V z, a), k) when Hashtbl.find_opt env z = Some Z && inv a ->
      Hashtbl.replace env x (A a); scan k
    | Let (x, U8, Load (V a), k) ->
      (match Hashtbl.find_opt env a with Some (A b) -> Hashtbl.replace env x (B b); scan k | _ -> None)
    | Store (U8, V a, v, k) when fin k ->
      (match Hashtbl.find_opt env a, v with
       | Some (A dst), V x when Hashtbl.mem env x && Hashtbl.find env x <> Inv ->
         (match Hashtbl.find env x with B src -> Some (dst, `Copy src) | P p -> Some (dst, `In p) | _ -> None)
       | Some (A dst), v when inv v -> Some (dst, `Fill v)
       | _ -> None)
    | Let (x, U8, In p, k) -> Hashtbl.replace env x (P p); scan k
    | Out (p, V x, k) when fin k ->
      (match Hashtbl.find_opt env x with Some (B src) -> Some (src, `Out p) | _ -> None)
    | Let (x, ty, e, k) when pure e -> Hashtbl.replace env x Inv; hoist := (x, ty, e) :: !hoist; scan k
    | _ -> None
  in
  Option.map (fun (d, w) -> d, w, List.rev !hoist) (scan t)

(* 変数 x が項の中で使われているか *)
let rec used x t =
  let a = function V y -> y = x | _ -> false in
  let e = function
    | Atom p | Zext p | Trunc p | Load p | Un (_, p) | Sh (_, p, _) | Mulc (p, _) | Idx (_, p) | Lut (_, p) -> a p
    | Bin (_, p, q) | Cmp (_, _, p, q) -> a p || a q
    | In _ -> false
  in
  match t with
  | Let (_, _, ex, k) -> e ex || used x k
  | Store (_, p, v, k) -> a p || a v || used x k
  | Out (_, v, k) -> a v || used x k
  | Call (_, _, args, k) -> List.exists (fun (p, _) -> a p) args || used x k
  | LetK (ks, b) -> List.exists (fun (k : kont) -> used x k.body) ks || used x b
  | Jump (_, args) -> List.exists a args
  | If (_, _, p, q, y, n) -> a p || a q || used x y || used x n
  | Switch (p, _, _) -> a p
  | Ret r -> (match r with Some (p, _) -> a p | None -> false)
  | Yield k -> used x k

let inv_bnd i = function V x -> x <> i | _ -> true

(* 回数 n の 16bit 化と、それを作る let *)
let wide = function
  | C n -> C n, (fun t -> t)
  | v -> let z = fresh () in V z, (fun t -> Let (z, U16, Zext v, t))

(* 番地 base + n - 1（下へ写すときの、最後のバイト） *)
let last base n =
  let add b c =
    match b with
    | C k -> C ((k + c) land 0xffff), (fun t -> t)
    | S (s, k) -> S (s, k + c), (fun t -> t)
    | V x -> let y = fresh () in V y, (fun t -> Let (y, U16, Bin ("+", V x, C (c land 0xffff)), t))
  in
  match n with
  | C k -> add base (k - 1)
  | v ->
    let z = fresh () and y = fresh () and y' = fresh () in
    V y', (fun t -> Let (z, U16, Zext v, Let (y, U16, Bin ("+", base, V z), Let (y', U16, Bin ("-", V y, C 1), t))))

(* 置き換え後の項。done へ渡す値 fin（使われなければ渡さない）、回数 n（0 なら何もしない）、
   dir は `Up か `Down *)
let make ~dir ~dn ~keep ~fin ~n (dst, what, hoists) =
  let pass = if keep then [ fin ] else [] in
  let small = match n with C k -> k < (match what with `Copy _ -> 2 | `Fill _ -> 4 | `Out _ | `In _ -> 3) | _ -> false in
  if small then None
  else begin
    let n16, bn = wide n in
    (* otir/inir の回数は B（8 ビット）。ポートは C *)
    let io op p a = Call (None, op, [ (C p, U8); (a, U16); (n, U8) ], Jump (dn, pass)) in
    let wrap, call =
      match what, dir with
      | `Out p, `Up -> (fun t -> t), io ".otir" p dst
      | `In p, `Up -> (fun t -> t), io ".inir" p dst
      | (`Out p | `In p), `Down ->
        let d, wd = last dst n in
        let op = (match what with `Out _ -> ".otdr" | _ -> ".indr") in
        wd, io op p d
      | `Copy src, `Up -> (fun t -> t), Call (None, ".ldir", [ (dst, U16); (src, U16); (n16, U16) ], Jump (dn, pass))
      | `Copy src, `Down ->
        let d, wd = last dst n and s, ws = last src n in
        (fun t -> wd (ws t)), Call (None, ".lddr", [ (d, U16); (s, U16); (n16, U16) ], Jump (dn, pass))
      | `Fill v, _ -> (fun t -> t), Call (None, ".fill", [ (dst, U16); (v, U8); (n16, U16) ], Jump (dn, pass))
    in
    let wrap t = List.fold_right (fun (x, ty, e) t -> Let (x, ty, e, t)) hoists (wrap t) in
    Some (match n with
          | C 0 -> Jump (dn, pass)
          | C _ -> wrap call
          | _ -> If ("==", U8, n, C 0, Jump (dn, pass), bn (wrap call)))
  end

(* LetK ([done], LetK ([loop], loop(start))) の形を探して、ループの代わりの項にする *)
let rewrite (dk : kont) (lk : kont) start : (kont * term) option =
  let keep = match dk.ps with [ (x, _) ] -> used x dk.body | _ -> true in
  let drop = if keep then dk else { dk with ps = [] } in
  match lk, start with
  | { ps = [ (i, U8) ]; body = If ("<", U8, V i1, n, LetK ([ nx ], stmts), Jump (dn, [ V i2 ])); _ }, C 0
    when i1 = i && i2 = i && dn = dk.k && inv_bnd i n ->
    (match nx.ps, nx.body with
     | [ (j, U8) ], Let (j1, U8, Bin ("+", V j2, C 1), Jump (lp, [ V j3 ])) when j2 = j && j3 = j1 && lp = lk.k ->
       let fin = function Jump (m, [ V i' ]) -> m = nx.k && i' = i | _ -> false in
       Option.bind (body i stmts fin) (fun w -> make ~dir:`Up ~dn ~keep ~fin:n ~n w)
       |> Option.map (fun t -> drop, t)
     | _ -> None)
  | { ps = [ (i, U8) ];
      body = If ("!=", U8, V i1, C 0, Let (t, U8, Bin ("-", V i2, C 1), stmts), Jump (dn, [ V i3 ])); _ }, n
    when i1 = i && i2 = i && i3 = i && dn = dk.k && inv_bnd i n ->
    let fin = function Jump (m, [ V t' ]) -> m = lk.k && t' = t | _ -> false in
    Option.bind (body t stmts fin) (fun w -> make ~dir:`Down ~dn ~keep ~fin:(C 0) ~n w)
    |> Option.map (fun t -> drop, t)
  | _ -> None

let rec go = function
  | LetK ([ dk ], (LetK ([ lk ], Jump (lp, [ start ])) as inner)) when lk.k = lp ->
    let dk = { dk with body = go dk.body } in
    (match rewrite dk lk start with
     | Some (dk, t) -> LetK ([ dk ], t)
     | None -> LetK ([ dk ], go inner))
  | LetK (ks, entry) -> LetK (List.map (fun (k : kont) -> { k with body = go k.body }) ks, go entry)
  | Let (x, ty, e, k) -> Let (x, ty, e, go k)
  | Store (ty, a, v, k) -> Store (ty, a, v, go k)
  | Out (p, v, k) -> Out (p, v, go k)
  | Call (r, f, args, k) -> Call (r, f, args, go k)
  | If (o, ty, a, b, y, n) -> If (o, ty, a, b, go y, go n)
  | Yield k -> Yield (go k)
  | (Jump _ | Switch _ | Ret _) as t -> t

let fn (f : fn) = { f with body = go f.body }
