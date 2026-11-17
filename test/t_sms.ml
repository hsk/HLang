(* SMS のテスト。examples/sms_game.hl を ROM にして、VDP・PSG・パッドを簡単に模したエミュレータで走らせ、
   VRAM や色、音のレジスタが狙いどおりになるかを確かめる。画面の割り込みは、HALT で止まるたびに入れる。 *)

open Hlang

let slurp f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let read f = slurp (if Sys.file_exists ("../lib/" ^ f) then "../lib/" ^ f else "../examples/" ^ f) in
  let r = compile ~target:sms ~read (read "sms_game.hl") in
  let image = rom r in
  let fails = ref 0 in
  let check what ok = if not ok then (incr fails; Printf.printf "NG sms: %s\n" what) in
  (* ROM の形 *)
  check "32KB" (String.length image = 0x8000);
  check "TMR SEGA" (String.sub image 0x7ff0 8 = "TMR SEGA");
  let sum = ref 0 in
  String.iteri (fun i c -> if i < 0x7ff0 then sum := (!sum + Char.code c) land 0xffff) image;
  check "チェックサム" (Char.code image.[0x7ffa] lor (Char.code image.[0x7ffb] lsl 8) = !sum);
  (* 機械 *)
  let s = Z80.make () in
  Z80.load s 0 image;
  let vram = Bytes.make 0x4000 '\xaa' and cram = Bytes.make 32 '\000' and reg = Array.make 16 0 in
  let first = ref true and low = ref 0 and addr = ref 0 and code = ref 0 in
  let psg = ref [] and pad = ref 0xff in
  s.out <- (fun port v ->
    match port land 0xff with
    | 0xbf ->
      if !first then low := v
      else begin
        code := v lsr 6;
        addr := ((v land 0x3f) lsl 8) lor !low;
        if !code = 2 then reg.(v land 15) <- !low
      end;
      first := not !first
    | 0xbe ->
      if !code = 3 then Bytes.set cram (!addr land 31) (Char.chr v) else Bytes.set vram (!addr land 0x3fff) (Char.chr v);
      incr addr;
      first := true
    | 0x7f -> psg := v :: !psg
    | _ -> ());
  s.inp <- (fun port -> match port land 0xff with 0xbf -> first := true; 0x80 | 0xdc -> !pad | _ -> 0xff);
  let frames n =
    for _ = 1 to n do
      let t = s.t + 2_000_000 in
      while not s.halt && s.t < t do Z80.step s done;
      check "1 フレームで HALT まで来る" s.halt;
      if reg.(1) land 0x20 <> 0 then Z80.irq s 0xff
    done
  in
  let v a = Char.code (Bytes.get vram a) in
  let spr n = v (0x3f00 + n), v (0x3f80 + (2 * n)), v (0x3f81 + (2 * n)) in     (* y, x, 絵 *)
  frames 1;
  check "モード 4" (reg.(0) = 0x04 && reg.(2) = 0xff && reg.(5) = 0xff && reg.(6) = 0xff);
  check "初代の VDP のために、レジスタ 3 と 4 は全ビット 1" (reg.(3) = 0xff && reg.(4) = 0xff);
  check "画面を出して、割り込みを起こす" (reg.(1) = 0x60);
  check "色" (Char.code (Bytes.get cram 15) = 0x3f && Char.code (Bytes.get cram 25) = 0x03);
  check "数字のタイル（色 15: 4 枚とも同じ）" (List.init 4 (fun i -> v ((48 * 32) + i)) = [ 0x3c; 0x3c; 0x3c; 0x3c ]);
  check "数字の 9 の 4 行目" (v ((57 * 32) + 12) = 0x3e);
  check "敵の絵（色 9: 0 枚目と 3 枚目）" (List.init 4 (fun i -> v (0x2020 + i)) = [ 0x42; 0; 0; 0x42 ]);
  check "VRAM の残りは空" (v 0x1000 = 0 && v 0x3000 = 0);
  check "使っていないスプライトは画面の外" (let y, _, _ = spr 63 in y = 0xe0);
  (* 最初の HALT は、メインループの頭。そこから 2 フレームで、自機と点数と最初の敵が出る *)
  frames 2;
  check "自機" (spr 0 = (168, 120, 0));
  check "点数は 00" (v (0x3800 + 64 + 2) = 48 && v (0x3800 + 64 + 4) = 48 && v (0x3800 + 64 + 3) = 0);
  check "敵が 1 匹出ている" (let y, x, p = spr 4 in y < 176 && x >= 40 && p = 1);
  (* 曲: 最初の音はチャンネル 0 に周期 428 *)
  let sent = List.rev !psg in
  let rec has = function a :: (b :: _ as rest) -> (a = 0x80 lor (428 land 15) && b = 428 lsr 4) || has rest | _ -> false in
  check "曲の最初の音" (has sent);
  check "音量" (List.mem (0x90 lor 5) sent && List.mem (0xb0 lor 5) sent);
  (* 右へ動く *)
  pad := 0xff land lnot 8;
  frames 10;
  check "右へ動く" (let _, x, _ = spr 0 in x = 140);
  (* 撃つ *)
  pad := 0xff land lnot 16;
  frames 2;
  check "弾が出る" (let y, x, p = spr 1 in y < 160 && x = 140 && p = 2);
  pad := 0xff;
  frames 60;
  check "弾は上で消える" (let y, _, _ = spr 1 in y = 0xe0);
  check "敵が降りてくる" (let y, _, _ = spr 4 in y > 60 && y < 176);
  (* 敵の真下で撃ち続ければ、点が入る *)
  for _ = 1 to 400 do
    let _, ex, _ = spr 4 and _, x, _ = spr 0 in
    pad := 0xff land lnot (16 lor (if x + 2 < ex then 8 else if x > ex + 2 then 4 else 0));
    frames 1
  done;
  check "当たると点が入る" (v (0x3800 + 64 + 4) <> 48 || v (0x3800 + 64 + 2) <> 48);
  Printf.printf "sms: %d バイト、点数 %c%c\n" (String.length r.bin) (Char.chr (v (0x3800 + 64 + 2))) (Char.chr (v (0x3800 + 64 + 4)));
  if !fails > 0 then exit 1
