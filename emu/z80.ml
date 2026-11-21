(* Z80 エミュレータ。小ささと見やすさを優先する。

   命令コードは 8 進数の 3 つの桁に分けて読む（xx yyy zzz）:

     x = 上位 2bit   命令の大分類
     y = 中央 3bit   レジスタ番号・演算の種類・条件
     z = 下位 3bit   レジスタ番号・命令の小分類
     p = y の上位 2bit  16bit レジスタ番号
     q = y の最下位 bit 同じ p での 2 択

   時間は T ステートで数える。命令ごとの表は持たず、バスの動きに課金する:

     命令フェッチ 4T、メモリ読み書き 3T、I/O 4T、内部動作は tick で明示

   フラグは非公式の bit3・bit5 まで実機に合わせる（zexall で確認）。 *)

type t = {
  m : Bytes.t;                   (* メモリ 64KB *)
  r : int array;                 (* B C D E H L F A IXh IXl IYh IYl *)
  r' : int array;                (* 裏レジスタ B' C' D' E' H' L' F' A' *)
  mutable hl : int;              (* いま HL 役をする組の位置: 4=HL 8=IX 10=IY *)
  mutable pc : int;
  mutable sp : int;
  mutable i : int;               (* 割り込みベクタの上位 *)
  mutable rf : int;              (* R: リフレッシュカウンタ *)
  mutable iff1 : bool;           (* 割り込み許可 *)
  mutable iff2 : bool;
  mutable im : int;              (* 割り込みモード *)
  mutable halt : bool;
  mutable t : int;               (* 経過 T ステート *)
  mutable inp : int -> int;      (* ポート番号 -> 値 *)
  mutable out : int -> int -> unit;  (* ポート番号 値 *)
}

(* r の 6 番は、命令コード上は (HL) を表すので空いている。そこに F を置く。 *)

let make () = {
  m = Bytes.make 0x10000 '\000'; r = Array.make 12 0; r' = Array.make 8 0;
  hl = 4; pc = 0; sp = 0xffff; i = 0; rf = 0;
  iff1 = false; iff2 = false; im = 0; halt = false; t = 0;
  inp = (fun _ -> 0xff); out = (fun _ _ -> ());
}

let load s org b = Bytes.blit_string b 0 s.m org (String.length b)

(* ---- レジスタ ---- *)

let a s = s.r.(7)
let f s = s.r.(6)
let seta s v = s.r.(7) <- v land 0xff
let setf s v = s.r.(6) <- v land 0xff

let rp s i = s.r.(i) lsl 8 lor s.r.(i + 1)          (* i: 0=BC 2=DE 4=HL *)
let set_rp s i v =
  s.r.(i) <- v lsr 8 land 0xff;
  s.r.(i + 1) <- v land 0xff

(* 8bit レジスタ番号。IX/IY 前置のとき H,L は IXh,IXl などに読み替える。 *)
let reg s i = if i = 4 || i = 5 then s.hl + i - 4 else i

(* 16bit レジスタ番号 p: 0=BC 1=DE 2=HL(IX,IY) 3=SP *)
let get16 s p = if p = 3 then s.sp else rp s (if p = 2 then s.hl else 2 * p)
let set16 s p v =
  if p = 3 then s.sp <- v land 0xffff
  else set_rp s (if p = 2 then s.hl else 2 * p) v

let swap s i =
  let v = s.r.(i) in
  s.r.(i) <- s.r'.(i);
  s.r'.(i) <- v

(* ---- バス ---- *)

let tick s n = s.t <- s.t + n

let rb s e = tick s 3; Char.code (Bytes.get s.m (e land 0xffff))
let wb s e v = tick s 3; Bytes.set s.m (e land 0xffff) (Char.chr (v land 0xff))

let rw s e = let l = rb s e in l lor (rb s (e + 1) lsl 8)
let ww s e v = wb s e v; wb s (e + 1) (v lsr 8)

let inb s port = tick s 4; s.inp port land 0xff
let outb s port v = tick s 4; s.out port v

(* 即値 n, nn *)
let imm s =
  let v = rb s s.pc in
  s.pc <- (s.pc + 1) land 0xffff;
  v
let imm16 s = let l = imm s in l lor (imm s lsl 8)

(* 命令フェッチ（M1 サイクル） *)
let m1 s =
  tick s 1;
  s.rf <- s.rf land 0x80 lor ((s.rf + 1) land 0x7f);
  imm s

let sx v = if v > 127 then v - 256 else v            (* 符号拡張 *)

(* 実効アドレス: (HL) または (IX+d) *)
let ea s =
  if s.hl = 4 then rp s 4
  else begin
    let d = sx (imm s) in
    tick s 5;
    (rp s s.hl + d) land 0xffff
  end

let push s v = s.sp <- (s.sp - 2) land 0xffff; ww s s.sp v
let pop s =
  let v = rw s s.sp in
  s.sp <- (s.sp + 2) land 0xffff;
  v

(* ---- フラグ ---- *)

let cf = 1 and nf = 2 and pf = 4 and hf = 16 and zf = 64 and sf = 128
let xf = 0x28                    (* 非公式の bit3, bit5。たいてい結果の写し *)

let on b c = if c then b else 0

let par v =                                           (* 1 の数が偶数か *)
  let v = v lxor (v lsr 4) in
  let v = v lxor (v lsr 2) in
  (v lxor (v lsr 1)) land 1 = 0

let sz v = v land (sf lor xf) lor on zf (v = 0)       (* v は 8bit *)
let xy s v = setf s (s.r.(6) land lnot xf lor (v land xf))   (* bit3,5 だけ v から *)
let szp v = sz v lor on pf (par v)

(* 条件 y: NZ Z NC C PO PE P M *)
let cc s y = (f s land [| zf; cf; pf; sf |].(y lsr 1) <> 0) = (y land 1 = 1)

(* ---- 8bit 演算 ---- *)

let add s v c =
  let x = a s in
  let r = x + v + c in
  setf s (sz (r land 0xff) lor on cf (r > 0xff)
          lor ((x lxor v lxor r) land hf)
          lor on pf ((x lxor r) land (v lxor r) land 0x80 <> 0));
  seta s r

(* 結果を返すだけで A には入れない。CP はフラグだけ使う。 *)
let sub s v c =
  let x = a s in
  let r = x - v - c in
  setf s (sz (r land 0xff) lor nf lor on cf (r < 0)
          lor ((x lxor v lxor r) land hf)
          lor on pf ((x lxor v) land (x lxor r) land 0x80 <> 0));
  r land 0xff

let logic s r h = seta s r; setf s (szp r lor h)

(* y: ADD ADC SUB SBC AND XOR OR CP *)
let alu s y v =
  let c = f s land cf in
  match y with
  | 0 -> add s v 0
  | 1 -> add s v c
  | 2 -> seta s (sub s v 0)
  | 3 -> seta s (sub s v c)
  | 4 -> logic s (a s land v) hf
  | 5 -> logic s (a s lxor v) 0
  | 6 -> logic s (a s lor v) 0
  | _ -> ignore (sub s v 0); xy s v                    (* CP: bit3,5 は相手から *)

let inc s v =
  let r = (v + 1) land 0xff in
  setf s (f s land cf lor sz r lor on hf (r land 15 = 0) lor on pf (r = 0x80));
  r

let dec s v =
  let r = (v - 1) land 0xff in
  setf s (f s land cf lor nf lor sz r
          lor on hf (r land 15 = 15) lor on pf (r = 0x7f));
  r

(* y: RLC RRC RL RR SLA SRA SLL SRL。結果と、押し出された bit を作る。 *)
let rot s y v =
  let c = f s land cf in
  let r, c =
    match y with
    | 0 -> v lsl 1 lor (v lsr 7), v lsr 7
    | 1 -> v lsr 1 lor (v lsl 7), v land 1
    | 2 -> v lsl 1 lor c, v lsr 7
    | 3 -> v lsr 1 lor (c lsl 7), v land 1
    | 4 -> v lsl 1, v lsr 7
    | 5 -> v lsr 1 lor (v land 0x80), v land 1
    | 6 -> v lsl 1 lor 1, v lsr 7
    | _ -> v lsr 1, v land 1
  in
  let r = r land 0xff in
  setf s (szp r lor c);
  r

let daa s =
  let x = a s and g = f s in
  let lo = x land 15 in
  let c = g land cf <> 0 || x > 0x99 in
  let h = g land hf <> 0 in
  let adj = on 6 (h || lo > 9) lor on 0x60 c in
  let n = g land nf <> 0 in
  let r = (if n then x - adj else x + adj) land 0xff in
  setf s (szp r lor (g land nf) lor on cf c
          lor on hf (if n then h && lo < 6 else lo > 9));
  seta s r

(* ---- 16bit 演算（結果は HL 役へ） ---- *)

let add16 s v =
  let x = get16 s 2 in
  let r = x + v in
  setf s (f s land (sf lor zf lor pf) lor on cf (r > 0xffff)
          lor ((x lxor v lxor r) lsr 8 land hf) lor (r lsr 8 land xf));
  set16 s 2 r

let adc16 s v =
  let x = rp s 4 in
  let r = x + v + (f s land cf) in
  setf s (r lsr 8 land (sf lor xf) lor on zf (r land 0xffff = 0) lor on cf (r > 0xffff)
          lor ((x lxor v lxor r) lsr 8 land hf)
          lor on pf ((x lxor r) land (v lxor r) land 0x8000 <> 0));
  set_rp s 4 r

let sbc16 s v =
  let x = rp s 4 in
  let r = x - v - (f s land cf) in
  setf s (r lsr 8 land (sf lor xf) lor on zf (r land 0xffff = 0) lor on cf (r < 0) lor nf
          lor ((x lxor v lxor r) lsr 8 land hf)
          lor on pf ((x lxor v) land (x lxor r) land 0x8000 <> 0));
  set_rp s 4 r

(* ---- 8bit オペランド: 番号 6 はメモリ ---- *)

let get8 s i = if i = 6 then rb s (ea s) else s.r.(reg s i)

(* 読んで、g を適用して、書き戻す *)
let upd s i g =
  if i = 6 then begin
    let e = ea s in
    let v = rb s e in
    tick s 1;
    wb s e (g v)
  end else begin
    let i = reg s i in
    s.r.(i) <- g s.r.(i)
  end

(* ---- 分岐 ---- *)

let jr s d = tick s 5; s.pc <- (s.pc + d) land 0xffff
let call s nn = tick s 1; push s s.pc; s.pc <- nn
let ret s = s.pc <- pop s

(* ---- CB: 回転・シフトとビット操作 ---- *)

let cb s =
  let ix = s.hl <> 4 in
  let d = if ix then sx (imm s) else 0 in             (* DD CB d op の順に並ぶ *)
  let op = if ix then (tick s 2; imm s) else m1 s in
  let e = (rp s s.hl + d) land 0xffff in
  let x = op lsr 6 and y = op lsr 3 land 7 and z = op land 7 in
  let mem = ix || z = 6 in
  let v = if mem then (let v = rb s e in tick s 1; v) else s.r.(z) in
  let put r = if mem then wb s e r else s.r.(z) <- r in
  let b = 1 lsl y in
  match x with
  | 0 -> put (rot s y v)
  | 1 ->                                                      (* BIT *)
    setf s (f s land cf lor hf lor (v land b land sf) lor on (zf lor pf) (v land b = 0));
    xy s (if mem then e lsr 8 else v)
  | 2 -> put (v land lnot b)                                  (* RES *)
  | _ -> put (v lor b)                                        (* SET *)

(* ---- ED: 拡張命令 ---- *)

(* ブロック命令。y: 4=進む 5=戻る 6=進んで繰り返す 7=戻って繰り返す *)
let block s y z =
  let dir = if y land 1 = 0 then 1 else -1 in
  let hl = rp s 4 and bc = rp s 0 in
  let b = (s.r.(0) - 1) land 0xff in
  let io () = s.r.(0) <- b; setf s (f s land cf lor nf lor on zf (b = 0)) in
  let again =
    match z with
    | 0 ->                                            (* LDI LDD LDIR LDDR *)
      let v = rb s hl in
      wb s (rp s 2) v;
      tick s 2;
      set_rp s 2 (rp s 2 + dir);
      set_rp s 0 (bc - 1);
      setf s (f s land (sf lor zf lor cf) lor on pf (bc <> 1));
      let n = v + a s in
      xy s (n land 8 lor (n lsl 4 land 32));          (* bit3 と、bit1 を bit5 へ *)
      bc <> 1
    | 1 ->                                            (* CPI CPD CPIR CPDR *)
      let c = f s land cf in
      let r = sub s (rb s hl) 0 in
      tick s 5;
      set_rp s 0 (bc - 1);
      setf s (f s land lnot (cf lor pf) lor c lor on pf (bc <> 1));
      let n = r - (f s land hf) lsr 4 in
      xy s (n land 8 lor (n lsl 4 land 32));
      bc <> 1 && r <> 0
    | 2 ->                                            (* INI IND INIR INDR *)
      tick s 1;
      let v = inb s bc in
      wb s hl v;
      io ();
      b <> 0
    | _ ->                                            (* OUTI OUTD OTIR OTDR *)
      tick s 1;
      let v = rb s hl in
      io ();
      outb s (rp s 0) v;
      b <> 0
  in
  set_rp s 4 (hl + dir);
  if y > 5 && again then (tick s 5; s.pc <- (s.pc - 2) land 0xffff)

(* RRD / RLD: A の下位 4bit と (HL) の間で 4bit ずつ回す *)
let rxd s left =
  let e = rp s 4 and x = a s in
  let v = rb s e in
  tick s 4;
  if left then (wb s e (v lsl 4 lor (x land 15)); seta s (x land 0xf0 lor (v lsr 4)))
  else (wb s e (x lsl 4 lor (v lsr 4)); seta s (x land 0xf0 lor (v land 15)));
  setf s (f s land cf lor szp (a s))

(* R800 の掛け算（MULUB、MULUW）を実行するか。Z80 では未定義の命令（NOP）なので、ふだんは false *)
let r800 = ref false

let ed s =
  let op = m1 s in
  let x = op lsr 6 and y = op lsr 3 land 7 and z = op land 7 in
  let p = y lsr 1 and q = y land 1 in
  match x, z with
  | 1, 0 ->                                           (* IN r,(C) *)
    let v = inb s (rp s 0) in
    if y <> 6 then s.r.(y) <- v;
    setf s (f s land cf lor szp v)
  | 1, 1 -> outb s (rp s 0) (if y = 6 then 0 else s.r.(y))
  | 1, 2 -> tick s 7; (if q = 0 then sbc16 else adc16) s (get16 s p)
  | 1, 3 ->
    let nn = imm16 s in
    if q = 0 then ww s nn (get16 s p) else set16 s p (rw s nn)
  | 1, 4 -> let v = a s in seta s 0; seta s (sub s v 0)           (* NEG *)
  | 1, 5 -> s.iff1 <- s.iff2; ret s                               (* RETN RETI *)
  | 1, 6 -> s.im <- [| 0; 0; 1; 2 |].(y land 3)
  | 1, _ ->
    let ld_a v =                                      (* LD A,I と LD A,R *)
      seta s v;
      setf s (f s land cf lor sz v lor on pf s.iff2)
    in
    (match y with
     | 0 -> tick s 1; s.i <- a s
     | 1 -> tick s 1; s.rf <- a s
     | 2 -> tick s 1; ld_a s.i
     | 3 -> tick s 1; ld_a s.rf
     | 4 -> rxd s false
     | 5 -> rxd s true
     | _ -> ())
  | 2, _ when z < 4 && y > 3 -> block s y z
  | 3, 1 when !r800 && y < 4 ->                       (* MULUB A,r: HL = A * r（r は B C D E） *)
    let v = a s * s.r.(y) in
    tick s 10;
    set16 s 2 v;
    setf s ((if v = 0 then zf else 0) lor (if v > 0xff then cf else 0))
  | 3, 3 when !r800 && q = 0 && (p = 0 || p = 3) ->   (* MULUW HL,BC / HL,SP: DE:HL = HL * rr *)
    let v = get16 s 2 * (if p = 0 then rp s 0 else s.sp) in
    tick s 32;
    set16 s 2 (v land 0xffff);
    set16 s 1 (v lsr 16);
    setf s ((if v = 0 then zf else 0) lor (if v > 0xffff then cf else 0))
  | _ -> ()                                           (* 未定義は NOP *)

(* ---- 前置なしの命令 ---- *)

let rec exec s =
  let op = m1 s in
  let x = op lsr 6 and y = op lsr 3 land 7 and z = op land 7 in
  let p = y lsr 1 and q = y land 1 in
  match x, z with
  | 0, 0 ->
    (match y with
     | 0 -> ()                                                    (* NOP *)
     | 1 -> swap s 6; swap s 7                                    (* EX AF,AF' *)
     | 2 ->                                                       (* DJNZ *)
       tick s 1;
       let d = sx (imm s) in
       s.r.(0) <- (s.r.(0) - 1) land 0xff;
       if s.r.(0) <> 0 then jr s d
     | 3 -> jr s (sx (imm s))                                     (* JR *)
     | _ -> let d = sx (imm s) in if cc s (y - 4) then jr s d)    (* JR cc *)
  | 0, 1 ->
    if q = 0 then set16 s p (imm16 s)                             (* LD rp,nn *)
    else (tick s 7; add16 s (get16 s p))                          (* ADD HL,rp *)
  | 0, 2 ->
    (match y with
     | 0 -> wb s (rp s 0) (a s)                                   (* LD (BC),A *)
     | 1 -> seta s (rb s (rp s 0))                                (* LD A,(BC) *)
     | 2 -> wb s (rp s 2) (a s)                                   (* LD (DE),A *)
     | 3 -> seta s (rb s (rp s 2))                                (* LD A,(DE) *)
     | 4 -> ww s (imm16 s) (get16 s 2)                            (* LD (nn),HL *)
     | 5 -> set16 s 2 (rw s (imm16 s))                            (* LD HL,(nn) *)
     | 6 -> wb s (imm16 s) (a s)                                  (* LD (nn),A *)
     | _ -> seta s (rb s (imm16 s)))                              (* LD A,(nn) *)
  | 0, 3 -> tick s 2; set16 s p (get16 s p + if q = 0 then 1 else -1)
  | 0, 4 -> upd s y (inc s)
  | 0, 5 -> upd s y (dec s)
  | 0, 6 ->                                                       (* LD r,n *)
    if y = 6 then begin
      let e = ea s in
      let n = imm s in
      if s.hl <> 4 then tick s (-3);       (* n の読み出しが番地計算と重なる *)
      wb s e n
    end else s.r.(reg s y) <- imm s
  | 0, _ ->
    let keep = f s land (sf lor zf lor pf) in
    (match y with
     | 4 -> daa s
     | 5 -> seta s (a s lxor 0xff); setf s (f s lor hf lor nf)    (* CPL *)
     | 6 -> setf s (keep lor cf)                                  (* SCF *)
     | 7 -> setf s (keep lor on hf (f s land cf <> 0)             (* CCF *)
                    lor ((f s lxor cf) land cf))
     | _ ->                                                       (* RLCA RRCA RLA RRA *)
       seta s (rot s y (a s));
       setf s (keep lor (f s land cf)));
    if y <> 4 then xy s (a s)
  | 1, _ when op = 0x76 -> s.halt <- true                         (* HALT *)
  | 1, _ ->                                                       (* LD r,r' *)
    if y = 6 then (let e = ea s in wb s e s.r.(z))
    else if z = 6 then (let e = ea s in s.r.(y) <- rb s e)
    else s.r.(reg s y) <- s.r.(reg s z)
  | 2, _ -> alu s y (get8 s z)
  | 3, 0 -> tick s 1; if cc s y then ret s                        (* RET cc *)
  | 3, 1 ->
    if q = 0 then begin                                           (* POP *)
      let v = pop s in
      if p = 3 then (seta s (v lsr 8); setf s v) else set16 s p v
    end else
      (match p with
       | 0 -> ret s
       | 1 -> for i = 0 to 5 do swap s i done                     (* EXX *)
       | 2 -> s.pc <- get16 s 2                                   (* JP (HL) *)
       | _ -> tick s 2; s.sp <- get16 s 2)                        (* LD SP,HL *)
  | 3, 2 -> let nn = imm16 s in if cc s y then s.pc <- nn         (* JP cc,nn *)
  | 3, 3 ->
    (match y with
     | 0 -> s.pc <- imm16 s                                       (* JP nn *)
     | 1 -> cb s
     | 2 -> let n = imm s in outb s (a s lsl 8 lor n) (a s)       (* OUT (n),A *)
     | 3 -> let n = imm s in seta s (inb s (a s lsl 8 lor n))     (* IN A,(n) *)
     | 4 ->                                                       (* EX (SP),HL *)
       let v = rw s s.sp in
       tick s 3;
       ww s s.sp (get16 s 2);
       set16 s 2 v
     | 5 ->                                                       (* EX DE,HL *)
       let de = rp s 2 in
       set_rp s 2 (rp s 4);
       set_rp s 4 de
     | 6 -> s.iff1 <- false; s.iff2 <- false                      (* DI *)
     | _ -> s.iff1 <- true; s.iff2 <- true)                       (* EI *)
  | 3, 4 -> let nn = imm16 s in if cc s y then call s nn          (* CALL cc,nn *)
  | 3, 5 ->
    if q = 0 then begin                                           (* PUSH *)
      tick s 1;
      push s (if p = 3 then a s lsl 8 lor f s else get16 s p)
    end else
      (match p with
       | 0 -> call s (imm16 s)
       | 1 -> s.hl <- 8; exec s                                   (* DD: IX *)
       | 2 -> s.hl <- 4; ed s
       | _ -> s.hl <- 10; exec s)                                 (* FD: IY *)
  | 3, 6 -> alu s y (imm s)
  | _ -> tick s 1; push s s.pc; s.pc <- y * 8                     (* RST *)

(* ---- 外から使うもの ---- *)

(* 1 命令実行する *)
let step s =
  if s.halt then tick s 4
  else (exec s; s.hl <- 4)

(* マスク可能割り込み。v はモード 2 でバスに乗る値。
   EI の直後 1 命令は割り込まない、という実機の遅延は再現しない。 *)
let irq s v =
  if s.iff1 then begin
    s.halt <- false;
    s.iff1 <- false;
    s.iff2 <- false;
    tick s 7;
    push s s.pc;
    s.pc <- if s.im = 2 then rw s (s.i lsl 8 lor v) else 0x38
  end
