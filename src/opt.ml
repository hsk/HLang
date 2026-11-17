(* 中間表現の上での小さな最適化: 同じ計算を 2 回しない。定数だけの計算は済ませておく。

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

(* 定数だけの計算は、ここで済ませる。ラベルの番地 + 定数も、1 つの値にまとめる（boss.y は boss+1 番地）。 *)
let fold ty e =
  let m = if ty = U8 then 0xff else 0xffff in
  let off c = let c = c land 0xffff in if c > 0x7fff then c - 0x10000 else c in
  let c v = Some (C (v land m)) in
  match e with
  | Bin ("+", C a, C b) -> c (a + b)
  | Bin ("-", C a, C b) -> c (a - b)
  | Bin ("&", C a, C b) -> c (a land b)
  | Bin ("|", C a, C b) -> c (a lor b)
  | Bin ("^", C a, C b) -> c (a lxor b)
  | Bin ("+", S (s, n), C k) | Bin ("+", C k, S (s, n)) when ty = U16 -> Some (S (s, n + off k))
  | Bin ("-", S (s, n), C k) when ty = U16 -> Some (S (s, n - off k))
  | Idx (s, C n) -> Some (S (s, n))
  | Sh ("<<", C a, n) -> c (a lsl n)
  | Sh (">>", C a, n) -> c ((a land m) lsr n)
  | Sh ("sar", C a, n) -> let a = a land m in c ((if a > m / 2 then a - m - 1 else a) asr n)
  | Mulc (C a, n) -> c (a * n)
  | Un ("-", C a) -> c (-a)
  | Un ("~", C a) -> c (lnot a)
  | Zext (C a) | Trunc (C a) -> Some (C (a land 0xff))
  | Cmp (o, t, C a, C b) ->
    let m = if t = U8 then 0xff else 0xffff in
    let a = a land m and b = b land m in
    Some (C (Bool.to_int (match o with "==" -> a = b | "!=" -> a <> b | "<" -> a < b | "<=" -> a <= b | ">" -> a > b | _ -> a >= b)))
  | _ -> None

(* 同じ「番地 + 定数」を指す変数 -> 最初にそれを計算した変数。読み込み済みかを調べるのに使う *)
let same : (string, string) Hashtbl.t = Hashtbl.create 16

let canon = function V m when Hashtbl.mem same m -> V (Hashtbl.find same m) | a -> a

(* sub: 置き換え（変数 -> 値）。pure: 計算済みの式。loads: 中身の分かっているメモリ（読んだ値か、書いた値）。 *)
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
    let e = match fold ty e with Some a -> Atom a | None -> e in
    (match e with
     | Atom a -> go ((x, a) :: sub) pure loads k
     | In _ -> Let (x, ty, e, go sub pure loads k)
     | Load a ->
       (* 同じ番地を指す別の変数で読んであれば、それを使う *)
       (match List.assoc_opt (ty, canon a) loads with
        | Some y -> go ((x, y) :: sub) pure loads k
        | None -> Let (x, ty, e, go sub pure (((ty, canon a), V x) :: loads) k))
     | Bin (("+" | "-"), V _, C _) when ty = U16 ->
       (* 番地 + 定数は使う所で計算し直す。直後の読み書きとまとめて、HL を進める形や (ix+d) にできる *)
       (match List.assoc_opt (ty, e) pure with
        | Some y -> Hashtbl.replace same x y; Let (x, ty, e, go sub pure loads k)
        | None -> Let (x, ty, e, go sub (((ty, e), x) :: pure) loads k))
     | _ -> reuse pure (fun p -> go sub (p :: pure) loads k))
  | Store (ty, a, v, k) ->
    (* 他の番地の中身は分からなくなるが、書いた番地の中身は分かっている *)
    Store (ty, at a, at v, go sub pure [ (ty, canon (at a)), at v ] k)
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
