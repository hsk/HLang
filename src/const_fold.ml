(* 定数畳み込み: 定数だけの計算を済ませる。ラベルの番地 + 定数も 1 つの値にまとめる。
   1 つの式を見て、畳めれば値を返す。呼ぶのは Cse（式を 1 つずつ見ながら使う）。 *)

open Ir

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
  | Sext (C a) -> let a = a land 0xff in Some (C (if a >= 0x80 then a lor 0xff00 else a))
  | Cmp (o, t, C a, C b) ->
    let m = if t = U8 then 0xff else 0xffff in
    let a = a land m and b = b land m in
    Some (C (Bool.to_int (match o with "==" -> a = b | "!=" -> a <> b | "<" -> a < b | "<=" -> a <= b | ">" -> a > b | _ -> a >= b)))
  | _ -> None
