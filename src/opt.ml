(* 中間表現の上での小さな最適化: 同じ計算を 2 回しない。

   変数は一度しか束縛されないので、同じ式はいつ計算しても同じ値になる。
   先に計算した変数がまだ見えている範囲なら、後の方をそれで置き換えられる。
   メモリの読みは、間に書き込みや関数呼び出しがなければ同じとみなす。
   （ポートの読みは毎回行う。読むたびに値が変わる番地を mem[] で読む場合は注意。） *)

open Ir

let map_exp f = function
  | Atom a -> Atom (f a)
  | Bin (o, a, b) -> Bin (o, f a, f b)
  | Sh (o, a, n) -> Sh (o, f a, n)
  | Mulc (a, n) -> Mulc (f a, n)
  | Idx (s, a) -> Idx (s, f a)
  | Lut (s, a) -> Lut (s, f a)
  | Un (o, a) -> Un (o, f a)
  | Zext a -> Zext (f a)
  | Trunc a -> Trunc (f a)
  | Cmp (o, t, a, b) -> Cmp (o, t, f a, f b)
  | Load a -> Load (f a)
  | In p -> In p

(* 同じ「番地 + 定数」を指す変数 -> 最初にそれを計算した変数。読み込み済みかを調べるのに使う *)
let same : (string, string) Hashtbl.t = Hashtbl.create 16

(* sub: 置き換え（変数 -> 値）。pure: 計算済みの式。loads: 読み込み済みのメモリ。 *)
let rec go sub pure loads t =
  let at = function V x -> (match List.assoc_opt x sub with Some a -> a | None -> V x) | a -> a in
  match t with
  | Let (x, ty, e, k) ->
    let e = map_exp at e in
    let reuse tbl keep =
      match List.assoc_opt (ty, e) tbl with
      | Some y -> go ((x, V y) :: sub) pure loads k
      | None -> Let (x, ty, e, keep ((ty, e), x))
    in
    (match e with
     | Atom a -> go ((x, a) :: sub) pure loads k
     | In _ -> Let (x, ty, e, go sub pure loads k)
     | Load a ->
       (* 同じ番地を指す別の変数で読んであれば、それを使う *)
       let key = ty, Load (match a with V m when Hashtbl.mem same m -> V (Hashtbl.find same m) | a -> a) in
       (match List.assoc_opt key loads with
        | Some y -> go ((x, V y) :: sub) pure loads k
        | None -> Let (x, ty, e, go sub pure ((key, x) :: loads) k))
     | Bin (("+" | "-"), V _, C _) when ty = U16 ->
       (* 番地 + 定数は使う所で計算し直す。直後の読み書きとまとめて、HL を進める形や (ix+d) にできる *)
       (match List.assoc_opt (ty, e) pure with
        | Some y -> Hashtbl.replace same x y; Let (x, ty, e, go sub pure loads k)
        | None -> Let (x, ty, e, go sub (((ty, e), x) :: pure) loads k))
     | _ -> reuse pure (fun p -> go sub (p :: pure) loads k))
  | Store (ty, a, v, k) -> Store (ty, at a, at v, go sub pure [] k)
  | Out (p, v, k) -> Out (p, at v, go sub pure [] k)
  | Call (r, f, args, k) -> Call (r, f, List.map (fun (a, t) -> at a, t) args, go sub pure [] k)
  | LetK (ks, b) ->
    (* 継続にはどこから飛んでくるか分からないので、メモリの内容は当てにしない *)
    LetK (List.map (fun (k : kont) -> { k with body = go sub pure [] k.body }) ks, go sub pure loads b)
  | Jump (k, args) -> Jump (k, List.map at args)
  | If (o, ty, a, b, y, n) -> If (o, ty, at a, at b, go sub pure loads y, go sub pure loads n)
  | Switch (a, cases, other) -> Switch (at a, cases, other)
  | Ret r -> Ret (Option.map (fun (a, t) -> at a, t) r)
  | Yield k -> Yield (go sub [] [] k)           (* 中断をまたいで値を持ち越さない *)

let fn (f : fn) = Hashtbl.reset same; { f with body = go [] [] [] f.body }
