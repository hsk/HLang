(* 小さなプログラムを走らせ、結果と T ステートを確かめる。 *)

let fails = ref 0

(* code を 0 番地から HALT まで実行し、(状態, HALT 手前までの T) を返す *)
let run ?(init = fun _ -> ()) code =
  let s = Z80.make () in
  Z80.load s 0 (code ^ "\x76");
  init s;
  while not s.halt do Z80.step s done;
  s, s.t - 4

let check name got want =
  if got <> want then begin
    incr fails;
    Printf.printf "NG %s: got %d (0x%x), want %d (0x%x)\n" name got got want want
  end

(* 1 命令の T ステート *)
let time name code want = check name (snd (run code)) want

let () =
  (* 8bit 演算とフラグ *)
  let s, _ = run "\x3e\x7f\xc6\x01" in                (* LD A,7Fh : ADD A,1 *)
  check "add a" (Z80.a s) 0x80;
  check "add f" (Z80.f s) 0x94;                       (* S H V *)
  let s, _ = run "\x3e\x10\xd6\x20" in                (* LD A,10h : SUB 20h *)
  check "sub a" (Z80.a s) 0xf0;
  check "sub f" (Z80.f s) 0xa3;                       (* S N C と、結果の bit5 *)
  let s, _ = run "\x3e\x15\xc6\x27\x27" in            (* 15 + 27 = 42 (BCD) *)
  check "daa" (Z80.a s) 0x42;
  let s, _ = run "\x37\x3f" in                        (* SCF : CCF *)
  check "ccf" (Z80.f s) 0x10;                         (* C が消えて H が立つ *)

  (* HL で配列を歩いて合計する: sum.loop(p: HL, n: B, acc: A) *)
  let s, t =
    run "\x21\x10\x00\x06\x04\xaf\x86\x23\x10\xfc"
      ~init:(fun s -> Z80.load s 0x10 "\x01\x02\x03\x04")
  in
  check "sum" (Z80.a s) 10;
  check "sum hl" (Z80.rp s 4) 0x14;
  check "sum t" t (10 + 7 + 4 + (7 + 6 + 13) * 3 + (7 + 6 + 8));

  (* CALL / RET とスタック *)
  let s, t = run "\x31\x00\x80\xcd\x08\x00\x76\x00\x3e\x05\xc9" in
  check "call a" (Z80.a s) 5;
  check "call sp" s.sp 0x8000;
  check "call t" t (10 + 17 + 7 + 10);

  (* IX は HL の読み替え *)
  let s, _ = run "\xdd\x21\x00\x40\xdd\x36\x05\x99\xdd\x7e\x05\xdd\x23" in
  check "ix a" (Z80.a s) 0x99;
  check "ix" (Z80.rp s 8) 0x4001;
  check "ix hl" (Z80.rp s 4) 0;

  (* LDIR *)
  let s, t =
    run "\x21\x20\x00\x11\x30\x00\x01\x03\x00\xed\xb0"
      ~init:(fun s -> Z80.load s 0x20 "abc")
  in
  check "ldir" (Char.code (Bytes.get s.m 0x32)) (Char.code 'c');
  check "ldir bc" (Z80.rp s 0) 0;
  check "ldir t" t (30 + 21 + 21 + 16);

  (* 割り込みモード 1 *)
  let s = Z80.make () in
  Z80.load s 0 "\x31\x00\x80\xed\x56\xfb\x76";       (* LD SP : IM 1 : EI : HALT *)
  Z80.load s 0x38 "\x3c\xc9";                         (* INC A : RET *)
  while not s.halt do Z80.step s done;
  Z80.irq s 0xff;
  Z80.step s; Z80.step s;
  check "irq a" (Z80.a s) 1;
  check "irq pc" s.pc 7;

  (* 命令ごとの T ステート。HL と IX の差がこの言語の出発点。 *)
  time "nop" "\x00" 4;
  time "ld a,(hl)" "\x7e" 7;
  time "ld a,(ix+d)" "\xdd\x7e\x01" 19;
  time "ld (hl),n" "\x36\x01" 10;
  time "ld (ix+d),n" "\xdd\x36\x01\x02" 19;
  time "inc (hl)" "\x34" 11;
  time "inc (ix+d)" "\xdd\x34\x01" 23;
  time "add a,(hl)" "\x86" 7;
  time "add a,(ix+d)" "\xdd\x86\x01" 19;
  time "inc hl" "\x23" 6;
  time "inc ix" "\xdd\x23" 10;
  time "add hl,de" "\x19" 11;
  time "adc hl,de" "\xed\x5a" 15;
  time "ld hl,(nn)" "\x2a\x00\x40" 16;
  time "ld bc,(nn)" "\xed\x4b\x00\x40" 20;
  time "ld a,(nn)" "\x3a\x00\x40" 13;
  time "push hl" "\xe5" 11;
  time "pop hl" "\xe1" 10;
  time "ex (sp),hl" "\xe3" 19;
  time "jp nn" "\xc3\x03\x00" 10;
  time "jr e" "\x18\x00" 12;
  time "jr nz (不成立)" "\xaf\x20\x00" (4 + 7);
  time "ret nz (不成立)" "\xaf\xc0" (4 + 5);
  time "rlc b" "\xcb\x00" 8;
  time "rlc (hl)" "\xcb\x06" 15;
  time "bit 0,(hl)" "\xcb\x46" 12;
  time "rlc (ix+d)" "\xdd\xcb\x01\x06" 23;
  time "bit 0,(ix+d)" "\xdd\xcb\x01\x46" 20;
  time "out (n),a" "\xd3\x00" 11;
  time "in a,(c)" "\xed\x78" 12;
  time "rld" "\xed\x6f" 18;
  time "neg" "\xed\x44" 8;
  time "ldi" "\xed\xa0" 16;
  time "cpi" "\xed\xa1" 16;
  time "outi" "\xed\xa3" 16;

  if !fails = 0 then print_endline "z80: ok" else exit 1
