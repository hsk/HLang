(* 構造体の配列を歩くポインタ（帰納変数の強度低減）。

     for b in 0..9 { ... enemies[b].x ... enemies[b].y ... }

   enemies[b] の番地は、そのままだと b * 17 の掛け算（シフトと足し算）で、使うたびに作る。
   そこで、ループに 16bit の引数 q を足し、入口で &enemies[0]、1 周ごとに q + 17 を渡す。
   本体の &enemies[b * 17] は q になる。掛け算と、join をまたいだ同じ計算の繰り返しが消える。

   ループの形（Cps の for が作る）:
     LetK ([done], LetK ([loop(.., i, ..)], loop(.., 開始, ..)))
     loop: if i < n then LetK ([next], <本体>) else ...
     next: let i' = i + 1; loop(.., i', ..)
   本体から next へ飛ぶ所（continue も）には q を足して渡す。
   前提は Ptr と同じで、添字が表の外を指さないこと（8bit の添字は一周するが、番地はしない）。
   掛け算のない &s[i] は、番地を作る手間が小さいので対象にしない。 *)

open Ir

let count = ref 0
let fresh () = incr count; Printf.sprintf "q.%d" !count

let rec index x i = function [] -> None | (y, _) :: r -> if y = x then Some i else index x (i + 1) r

(* 本体を書き換える: &s[i*k] を q に、next へのジャンプに q を足す *)
let rec rewrite muls qs nx = function
  | Let (x, U16, Idx (s, V t), k) when Hashtbl.mem muls t && List.mem_assoc (s, Hashtbl.find muls t) qs ->
    Let (x, U16, Atom (V (List.assoc (s, Hashtbl.find muls t) qs)), rewrite muls qs nx k)
  | Let (x, ty, e, k) -> Let (x, ty, e, rewrite muls qs nx k)
  | Store (ty, a, v, k) -> Store (ty, a, v, rewrite muls qs nx k)
  | Out (p, v, k) -> Out (p, v, rewrite muls qs nx k)
  | Call (r, f, args, k) -> Call (r, f, args, rewrite muls qs nx k)
  | LetK (ks, b) -> LetK (List.map (fun (k : kont) -> { k with body = rewrite muls qs nx k.body }) ks, rewrite muls qs nx b)
  | Jump (k, args) when k = nx -> Jump (k, args @ List.map (fun (_, q) -> V q) qs)
  | If (o, ty, a, b, y, n) -> If (o, ty, a, b, rewrite muls qs nx y, rewrite muls qs nx n)
  | Yield k -> Yield (rewrite muls qs nx k)
  | (Jump _ | Switch _ | Ret _) as t -> t

(* 本体で、i の定数倍（Mulc か <<）を添字にした表の番地を集める。(表, 倍数) の並び *)
let uses i body =
  let muls = Hashtbl.create 8 and found = ref [] in
  let rec scan = function
    | Let (x, U8, Mulc (V v, k), r) when v = i && k > 1 -> Hashtbl.replace muls x k; scan r
    | Let (x, U8, Sh ("<<", V v, n), r) when v = i && n >= 1 && n <= 7 -> Hashtbl.replace muls x (1 lsl n); scan r
    | Let (_, U16, Idx (s, V t), r) ->
      (match Hashtbl.find_opt muls t with
       | Some k when not (List.mem (s, k) !found) -> found := (s, k) :: !found
       | _ -> ());
      scan r
    | Let (_, _, _, r) | Store (_, _, _, r) | Out (_, _, r) | Call (_, _, _, r) | Yield r -> scan r
    | LetK (ks, b) -> List.iter (fun (k : kont) -> scan k.body) ks; scan b
    | If (_, _, _, _, y, n) -> scan y; scan n
    | Jump _ | Switch _ | Ret _ -> ()
  in
  scan body;
  muls, List.rev !found

(* loop を、q を持ち歩く形にする *)
let walk (lk : kont) starts =
  match lk.body with
  | If ("<", U8, V i, bnd, LetK ([ nx ], stmts), els) ->
    (match index i 0 lk.ps, nx.body with
     | Some pi, Let (i', U8, Bin ("+", V ix, C 1), Jump (lp, args))
       when lp = lk.k && List.length args = List.length lk.ps && List.nth args pi = V i' && List.mem_assoc ix nx.ps ->
       (match List.nth starts pi with
        | C s0 ->
          let muls, found = uses i stmts in
          if found = [] then None
          else begin
            let qs = List.map (fun sk -> sk, fresh ()) found in            (* loop の引数 *)
            let qn = List.map (fun sk -> sk, fresh ()) found in            (* next の引数 *)
            let qn' = List.map (fun sk -> sk, fresh ()) found in           (* next で進めた値 *)
            let step =
              List.fold_right
                (fun ((sk, q), (_, q')) t -> Let (q', U16, Bin ("+", V q, C (snd sk)), t))
                (List.combine qn qn')
                (Jump (lp, args @ List.map (fun (_, q') -> V q') qn'))
            in
            let nx = { nx with ps = nx.ps @ List.map (fun (_, q) -> q, U16) qn; body = Let (i', U8, Bin ("+", V ix, C 1), step) } in
            let body = If ("<", U8, V i, bnd, LetK ([ nx ], rewrite muls qs nx.k stmts), els) in
            let lk = { lk with ps = lk.ps @ List.map (fun (_, q) -> q, U16) qs; body } in
            Some (lk, starts @ List.map (fun ((s, k), _) -> S (s, s0 * k)) qs)
          end
        | _ -> None)
     | _ -> None)
  | _ -> None

(* 添字が回数を数えるだけになったループ（for i in 0..n で、本体が i を使わない）は、n から 0 へ数え下ろす。
   判定が「0 でないか」になり、dec のフラグで決まる。B に置けば djnz 1 つ（13T）。
   上へ数えると inc b / ld a,b / cp n / jp c（25T）かかり、A も使う。抜けた後の i は n。 *)
(* 項の中のジャンプを置き換える *)
let rec map_jumps f = function
  | Let (x, ty, e, k) -> Let (x, ty, e, map_jumps f k)
  | Store (ty, a, v, k) -> Store (ty, a, v, map_jumps f k)
  | Out (p, v, k) -> Out (p, v, map_jumps f k)
  | Call (r, g, args, k) -> Call (r, g, args, map_jumps f k)
  | LetK (ks, b) -> LetK (List.map (fun (k : kont) -> { k with body = map_jumps f k.body }) ks, map_jumps f b)
  | Jump (k, args) -> f k args
  | If (o, ty, a, b, y, n) -> If (o, ty, a, b, map_jumps f y, map_jumps f n)
  | Yield k -> Yield (map_jumps f k)
  | (Switch _ | Ret _) as t -> t

(* let を並べた後の、最後のジャンプ *)
let rec final = function Let (_, _, _, k) -> final k | Jump (k, args) -> Some (k, args) | _ -> None

let down (lk : kont) starts =
  match lk.body with
  | If ("<", U8, V i, C n, LetK ([ nx ], stmts), (Jump (dn, eargs) as _els)) when n >= 1 && n <= 255 ->
    (match index i 0 lk.ps, nx.body with
     | Some pi, Let (i', U8, Bin ("+", V ix, C 1), j)
       when (match final j with Some (lp, args) -> lp = lk.k && List.nth args pi = V i' | None -> false)
            && List.nth starts pi = C 0 && List.mem_assoc ix nx.ps
            && (let nxp = match index ix 0 nx.ps with Some k -> k | None -> -1 in
                (* next へ i をそのまま渡す所は、数えるのに使っているだけ *)
                let strip = map_jumps (fun k args ->
                    Jump (k, if k = nx.k then List.mapi (fun j a -> if j = nxp && a = V i then C 0 else a) args else args)) in
                not (Blk.used i (strip stmts))) ->
       let eargs = List.map (fun a -> if a = V i then C n else a) eargs in
       (* 数を減らすのは最後にする。dec の直後に飛ぶので、djnz 1 つになる *)
       let rec last = function
         | Let (x, ty, e, k) -> Let (x, ty, e, last k)
         | t -> Let (i', U8, Bin ("-", V ix, C 1), t)
       in
       let nx = { nx with body = last j } in
       let body = If ("!=", U8, V i, C 0, LetK ([ nx ], stmts), Jump (dn, eargs)) in
       Some ({ lk with body }, List.mapi (fun k a -> if k = pi then C n else a) starts)
     | _ -> None)
  | _ -> None

let rec go = function
  | LetK ([ dk ], LetK ([ lk ], Jump (lp, starts))) when lk.k = lp ->
    let dk = { dk with body = go dk.body } in
    let lk = { lk with body = go lk.body } in
    let lk, starts = match walk lk starts with Some r -> r | None -> lk, starts in
    let lk = { lk with body = Narrow.drop lk.body } in        (* 置き換えで使われなくなった掛け算を消す *)
    (match down lk starts with
     | Some (lk, starts) ->
       (* 抜けた後の i が使われないなら、done の引数から外す（定数を置き場所へ入れる手間が要らない） *)
       let unused = List.mapi (fun j (x, _) -> j, not (Blk.used x dk.body)) dk.ps in
       let keep j = not (List.assoc j unused) in
       let dk = { dk with ps = List.filteri (fun j _ -> keep j) dk.ps } in
       let lk = { lk with body = map_jumps (fun k args ->
           Jump (k, if k = dk.k then List.filteri (fun j _ -> keep j) args else args)) lk.body } in
       LetK ([ dk ], LetK ([ lk ], Jump (lp, starts)))
     | None -> LetK ([ dk ], LetK ([ lk ], Jump (lp, starts))))
  | LetK (ks, entry) -> LetK (List.map (fun (k : kont) -> { k with body = go k.body }) ks, go entry)
  | Let (x, ty, e, k) -> Let (x, ty, e, go k)
  | Store (ty, a, v, k) -> Store (ty, a, v, go k)
  | Out (p, v, k) -> Out (p, v, go k)
  | Call (r, f, args, k) -> Call (r, f, args, go k)
  | If (o, ty, a, b, y, n) -> If (o, ty, a, b, go y, go n)
  | Yield k -> Yield (go k)
  | (Jump _ | Switch _ | Ret _) as t -> t

let fn (f : fn) = { f with body = go f.body }
