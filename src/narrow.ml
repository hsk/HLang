(* 16bit の計算を 8bit に狭める。

   言語は u8 と u16 を混ぜると u16 で計算する。たとえば
     hl += ((ix.angle + 0x20) & 0xc0) >> 3
   は、zext してから 16bit で足し、16bit で and し、16bit で 3 回シフトする（srl h / rr l を 3 回）。
   0xc0 で切り取った後は上位が 0 なので、下位だけを 8bit で計算すれば済む。

   1. x & m（m <= 0xff）: 下位バイトだけを 8bit で計算して、zext する。足し算・引き算・論理演算・左シフトの
      下位バイトは、元の値の下位バイトだけで決まる。
   2. (zext b) >> n: b >> n を 8bit で計算して、zext する。
   3. (mem16[a] >> 8) を使うのが 1 か所だけなら、mem[a + 1] の 1 バイトを読む（上位バイトはメモリでは後ろ）。
   4. trunc (zext b) は b。
   置き換えた後に使われなくなった読み（mem16[a]）は消す。 *)

open Ir

let count = ref 0
let fresh () = incr count; Printf.sprintf "n.%d" !count

let atoms_e = function
  | Atom p | Zext p | Trunc p | Load p | Un (_, p) | Sh (_, p, _) | Mulc (p, _) | Idx (_, p) | Lut (_, p) -> [ p ]
  | Bin (_, p, q) | Cmp (_, _, p, q) -> [ p; q ]
  | In _ -> []

(* 変数ごとの使われる回数 *)
let uses body =
  let n = Hashtbl.create 64 in
  let a = function V x -> Hashtbl.replace n x (1 + Option.value ~default:0 (Hashtbl.find_opt n x)) | _ -> () in
  let rec go = function
    | Let (_, _, e, k) -> List.iter a (atoms_e e); go k
    | Store (_, p, v, k) -> a p; a v; go k
    | Out (_, v, k) -> a v; go k
    | Call (_, _, args, k) -> List.iter (fun (p, _) -> a p) args; go k
    | LetK (ks, b) -> List.iter (fun (k : kont) -> go k.body) ks; go b
    | Jump (_, args) -> List.iter a args
    | If (_, _, p, q, y, n) -> a p; a q; go y; go n
    | Switch (p, _, _) -> a p
    | Ret r -> Option.iter (fun (p, _) -> a p) r
    | Yield k -> go k
  in
  go body;
  fun x -> Option.value ~default:0 (Hashtbl.find_opt n x)

let id t = t

let narrow body =
  let used = uses body in
  let defs = Hashtbl.create 64 in
  let def = function V x -> Hashtbl.find_opt defs x | _ -> None in
  let let8 e = let z = fresh () in (fun t -> Let (z, U8, e, t)), V z in
  (* 16bit の値 p の下位バイトを作る。(let を足す関数, 値) *)
  let rec lo8 p =
    match p, def p with
    | C n, _ -> id, C (n land 0xff)
    | _, Some (U16, Zext b) -> id, b
    | _, Some (U16, Bin ((("+" | "-" | "&" | "|" | "^") as o), q, r)) ->
      let w1, a1 = lo8 q in
      let w2, a2 = lo8 r in
      let w3, z = let8 (Bin (o, a1, a2)) in
      (fun t -> w1 (w2 (w3 t))), z
    | _, Some (U16, Sh ("<<", q, n)) ->
      let w1, a1 = lo8 q in
      let w2, z = let8 (Sh ("<<", a1, n)) in
      (fun t -> w1 (w2 t)), z
    | _ -> let8 (Trunc p)
  in
  (* 番地 a + 1 *)
  let next a =
    match a, def a with
    | S (s, n), _ -> id, S (s, n + 1)
    | C n, _ -> id, C ((n + 1) land 0xffff)
    | _, Some (U16, Bin ("+", (V _ as b), C j)) ->
      let z = fresh () in (fun t -> Let (z, U16, Bin ("+", b, C ((j + 1) land 0xffff)), t)), V z
    | _ -> let z = fresh () in (fun t -> Let (z, U16, Bin ("+", a, C 1), t)), V z
  in
  let rec go = function
    | Let (x, ty, e, k) ->
      let bind e = Hashtbl.replace defs x (ty, e); Let (x, ty, e, go k) in
      (match ty, e with
       | U16, Bin ("&", C m, p) when m <= 0xff -> go (Let (x, ty, Bin ("&", p, C m), k))
       | U16, Bin ("&", p, C m) when m <= 0xff ->
         let w, r = lo8 p in
         let w2, z = let8 (Bin ("&", r, C m)) in
         w (w2 (bind (Zext z)))
       | U16, Sh (">>", y, n) when n >= 1 && (match def y with Some (U16, Zext _) -> true | _ -> false) ->
         let b = match def y with Some (_, Zext b) -> b | _ -> assert false in
         let w, z = let8 (Sh (">>", b, n)) in
         w (bind (Zext z))
       | U16, Sh (">>", (V m as y), 8) when used m = 1 && (match def y with Some (U16, Load _) -> true | _ -> false) ->
         let a = match def y with Some (_, Load a) -> a | _ -> assert false in
         let w, a1 = next a in
         let w2, z = let8 (Load a1) in
         w (w2 (bind (Zext z)))
       | U8, Trunc y when (match def y with Some (U16, Zext _) -> true | _ -> false) ->
         let b = match def y with Some (_, Zext b) -> b | _ -> assert false in
         bind (Atom b)
       | _ -> bind e)
    | Store (ty, a, v, k) -> Store (ty, a, v, go k)
    | Out (p, v, k) -> Out (p, v, go k)
    | Call (r, f, args, k) -> Call (r, f, args, go k)
    | LetK (ks, b) -> LetK (List.map (fun (k : kont) -> { k with body = go k.body }) ks, go b)
    | If (o, ty, a, b, y, n) -> If (o, ty, a, b, go y, go n)
    | Yield k -> Yield (go k)
    | (Jump _ | Switch _ | Ret _) as t -> t
  in
  go body

(* 使われない計算と読みを消す（メモリの読みは、ほかに何も起こさない。ポートの読みは残す）。
   消すと、その材料も使われなくなることがあるので、減らなくなるまで繰り返す。 *)
let rec drop body =
  let used = uses body in
  let gone = ref false in
  let rec go = function
    | Let (x, _, e, k) when used x = 0 && (match e with In _ -> false | _ -> true) -> gone := true; go k
    | Let (x, ty, e, k) -> Let (x, ty, e, go k)
    | Store (ty, a, v, k) -> Store (ty, a, v, go k)
    | Out (p, v, k) -> Out (p, v, go k)
    | Call (r, f, args, k) -> Call (r, f, args, go k)
    | LetK (ks, b) -> LetK (List.map (fun (k : kont) -> { k with body = go k.body }) ks, go b)
    | If (o, ty, a, b, y, n) -> If (o, ty, a, b, go y, go n)
    | Yield k -> Yield (go k)
    | (Jump _ | Switch _ | Ret _) as t -> t
  in
  let b = go body in
  if !gone then drop b else b

let fn (f : fn) = { f with body = drop (narrow f.body) }
