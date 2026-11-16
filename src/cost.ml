(* 命令のコスト。表は持たず、組み立てた長さと、エミュレータで実際に走らせた時間を使う。
   アセンブラ・エミュレータと食い違うことがない。 *)

let size ins = String.length (fst (Asm.assemble ~org:0x100 [ Asm.Ins ins ]))

(* T ステートの (最短, 最長)。条件が成立する場合としない場合の両方を走らせる。 *)
let time ins =
  let bin, _ = Asm.assemble ~org:0x100 [ Asm.Ins ins ] in
  let run f bc =
    let s = Z80.make () in
    Bytes.fill s.m 0 0x10000 '\x55';
    Z80.load s 0x100 bin;
    s.pc <- 0x100;
    s.sp <- 0x8000;
    Z80.setf s f;
    Z80.set_rp s 0 bc;
    Z80.step s;
    s.t
  in
  let t1 = run 0x00 0x0001 and t2 = run 0xff 0x0102 in
  min t1 t2, max t1 t2

(* ((T ステートの最短, 最長), バイト数)。命令の形ごとに 1 回だけ測って覚えておく。 *)
let measured : (string * Asm.arg list, (int * int) * int) Hashtbl.t = Hashtbl.create 256

let rec norm : Asm.arg -> Asm.arg = function
  | I _ -> I (Num 0x4010)
  | M x -> M (norm x)
  | X (r, _) -> X (r, Num 1)
  | x -> x

let measure (op, args) =
  let key = op, List.map norm args in
  let key = match op, args with ("bit" | "set" | "res"), n :: rest -> op, n :: List.map norm rest | _ -> key in
  (* 相対ジャンプは近くへ飛ぶ形で測る（測るときは 0x100 番地に置く） *)
  let key = if op = "jr" || op = "djnz" then op, List.map (function Asm.I _ -> Asm.I (Num 0x102) | x -> x) args else key in
  match Hashtbl.find_opt measured key with
  | Some m -> m
  | None ->
    let m = time key, size key in
    Hashtbl.replace measured key m;
    m

(* 近くへ飛ぶジャンプを短い形にする。番地は命令の大きさを足して求める（縮む前の番地で測るので控えめ）。
   - dec b / jp nz は djnz 1 つにする。
   - 前へ飛ぶ条件付きの jp は jr にする。成り立たないとき 7 T で済み（jp は 10 T）、1 バイト短い。
     成り立つと 12 T かかるので、後ろへ飛ぶもの（ループの戻り。たいてい成り立つ）と無条件のものは jp のまま。 *)
let short code =
  let at = Hashtbl.create 16 in
  let size = function Asm.Ins i -> snd (measure i) | _ -> 0 in
  ignore (List.fold_left (fun pc (l, _) -> (match l with Asm.Label s -> Hashtbl.replace at s pc | _ -> ()); pc + size l) 0 code);
  let dist pc l = match Hashtbl.find_opt at l with Some t -> t - pc | None -> max_int in
  let rec go pc = function
    | (Asm.Ins ("dec", [ R "b" ]), d) :: (Asm.Ins ("jp", [ R "nz"; I (Sym l) ]), _) :: rest when abs (dist pc l) < 120 ->
      (Asm.Ins ("djnz", [ I (Sym l) ]), d) :: go (pc + 4) rest
    | (Asm.Ins ("jp", [ R (("z" | "nz" | "c" | "nc") as cc); I (Sym l) ]), d) :: rest when dist pc l > 0 && dist pc l < 120 ->
      (Asm.Ins ("jr", [ R cc; I (Sym l) ]), d) :: go (pc + 3) rest
    | ((l, _) as x) :: rest -> x :: go (pc + size l) rest
    | [] -> []
  in
  go 0 code

(* コードの値段: 実行時間と大きさ。行には「何回実行されそうか」の重み w が付いている。
   ループの中は 1 段につき 8 倍（4 段まで）、分岐の先はそこを通る割合だけ。
   時間に幅がある命令（条件付きの jr や ret、djnz）は、長い方（飛ぶ方）になる回数を次の行の重みから
   求める。次の行の方が軽ければ、減った分が飛んだ回数。分からなければ半々とみなす。
   表は 256 バイト境界に置くので、埋め草を見込んで 1 つ 256 バイトと数える。 *)
let price code data =
  let cap = Float.min 4096. in
  let rec go sum = function
    | (Asm.Ins i, w) :: rest ->
      let (lo, hi), n = measure i and w = cap w in
      let taken = match rest with (Asm.Ins _, w') :: _ when cap w' < w -> w -. cap w' | _ -> w /. 2. in
      go (sum +. (4. *. ((float lo *. w) +. (float (hi - lo) *. taken))) +. float n) rest
    | _ :: rest -> go sum rest
    | [] -> sum
  in
  List.fold_left (fun n l -> match l with Asm.Align k -> n + k | _ -> n) (int_of_float (go 0. code)) data
