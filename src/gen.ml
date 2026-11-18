(* コード生成。中間表現を Z80 の命令列にする。

   変数にはそれぞれ「置き場所」がある。レジスタ（a b c d e h l / bc de hl）か、固定番地のメモリ。
   再帰がないのでスタックフレームは要らない。関数の引数の置き場所が、そのまま呼び出し規約になる。

   このファイルは「置き場所の割り当てが与えられたら、正しいコードを出す。出せないなら Stuck」
   までを受け持つ。レジスタを書き換える命令を出すたびに、生きている変数の置き場所を壊さないか
   調べ、壊すなら Stuck を投げる。どの割り当てが良いかは Alloc が試して決める。
   全部メモリに置く割り当てなら、必ず成功する。 *)

open Ir
open Asm
module L = Live
module SS = L.SS

exception Stuck of string           (* 理由。調べるとき用 *)

(* 呼ぶ側が知っておくこと: 引数の置き場所と、壊されるレジスタ *)
type info = { phome : string list; clob : string list }

type loc =
  | Reg of string
  | Slot of string               (* "@ラベル": メモリ上の置き場所 *)
  | Imm of e

type ctx = {
  f : string;
  task : bool;                                         (* タスクの本体か *)
  self : string option;                                (* プールのタスク: フレームを指す変数 *)
  home : string -> string;                             (* 変数 -> レジスタ名か "@ラベル" *)
  konts : (string, (string * ty) list * SS.t) Hashtbl.t;   (* 継続 -> (引数, 入口で生きている変数) *)
  info : (string, info) Hashtbl.t;
  exts : (string * ((Ast.ty * string) list * Ast.ty * int * string list)) list;
  mutable out : (line * float) list;                   (* (行, 重み) を逆順に *)
  mutable w : float;                                   (* 重み: いま出している行が何回実行されそうか *)
  mutable busy : string list;                          (* いま壊してはいけない置き場所 *)
  mutable ca : atom option;                            (* A に入っていると分かっている値 *)
  mutable chl : atom option;                           (* HL に入っていると分かっている値 *)
  mutable hnear : (string * int) option;               (* HL が「この変数 + n」を指していると分かっている *)
  mutable zf : string option;                          (* Z フラグが「この変数は 0」を表している *)
  mutable n : int;
  (* 先頭で条件を調べるループ: 継続 -> (比較, 型, 左, 右, 本体のラベル, 抜けるときの項) *)
  heads : (string, string * ty * atom * atom * string * L.t) Hashtbl.t;
  mutable norot : bool;
  pick : bool;                                         (* 値での分岐の出し方を、測って選ぶか *)
  mutable ofs : (string * int) list;                   (* 歩いているポインタ（HL, DE, BC）が、本来の値からいくつ進んでいるか *)
  mutable walking : bool;                              (* 進んだままのポインタを使う命令を出している最中 *)
  mutable data : line list;                            (* 関数の外に置く表 *)
  bodies : (string, L.t * float) Hashtbl.t;            (* 継続 -> (本体, そこの重み) *)
  mutable wsel : float option;                         (* 複製の中: 分岐の出し方は、元の場所の重みで選ぶ *)
  mutable dup : bool;                                  (* 継続の本体を、飛ぶ代わりに複製している最中 *)
  mutable tabs : ((int * string) list * string option * bool * string) list;   (* 出した飛び先の表（分岐の値、既定の飛び先、2 ページ版か、表の名前）。複製どうしで共有する *)
  mutable next : (line * float) list Lazy.t;           (* いま出している項のすぐ後ろに並ぶコード（近い分だけ） *)
  mutable last : string option;                        (* 計算してすぐ返すか渡すだけの変数。置き場所へは書かず、A か HL から使う *)
  mutable held : (string * ty) option;                 (* 置き場所へ書かずに、A か HL にだけ持っている変数 *)
}

(* いまの状態を覚えて、そこへ戻す関数を返す *)
let mark c =
  let out, ca, chl, zf, n, busy, ofs, w, data = c.out, c.ca, c.chl, c.zf, c.n, c.busy, c.ofs, c.w, c.data in
  let norot, tabs, dup, wsel, hnear, held = c.norot, c.tabs, c.dup, c.wsel, c.hnear, c.held in
  fun () ->
    c.held <- held;
    c.norot <- norot; c.tabs <- tabs; c.dup <- dup; c.wsel <- wsel; c.hnear <- hnear;
    c.out <- out; c.ca <- ca; c.chl <- chl; c.zf <- zf; c.n <- n; c.busy <- busy; c.ofs <- ofs;
    c.w <- w; c.data <- data; c.walking <- false

(* 試しに出してみて、行き詰まったら出した分を取り消して別の方法で出す *)
let attempt c f g =
  let back = mark c in
  try f () with Stuck _ -> back (); g ()

(* 出してみて (値段, バイト数) を測り、出す前に戻す。行き詰まるなら None。
   後ろに並ぶコードをつないで測るので、近くへ飛ぶ分岐は jr の値段になる。 *)
let trial c f =
  let back = mark c and out = c.out and data = c.data in
  let rec since l = if l == out then [] else List.hd l :: since (List.tl l) in
  let r =
    try
      f ();
      let code = List.rev (since c.out) in
      let bytes = List.fold_left (fun n (l, _) -> match l with Ins i -> n + snd (Cost.measure i) | _ -> n) 0 code in
      Some (Cost.price (Cost.short (code @ Lazy.force c.next)) (List.filter (fun d -> not (List.memq d data)) c.data), bytes)
    with Stuck _ -> None
  in
  back ();
  r

let a = R "a" and hl = R "hl"
let is_x r = r = "ix" || r = "iy"
let hi r = if is_x r then raise (Stuck "IX/IY は半分ずつ使わない") else String.sub r 0 1
let lo r = if is_x r then raise (Stuck "IX/IY は半分ずつ使わない") else String.sub r 1 1
let is_mem h = h.[0] = '@'
let addr h = Sym (String.sub h 1 (String.length h - 1))
let places h = if is_mem h then [ h ] else halves h   (* 置き場所が占める場所 *)

let set_live c lv = c.busy <- SS.fold (fun x l -> places (c.home x) @ l) lv []
let guard c ws =
  match List.find_opt (fun w -> List.mem w c.busy) ws with
  | Some w -> raise (Stuck ("生きている値を壊す: " ^ w))
  | None -> ()
let free c r = not (List.exists (fun w -> List.mem w c.busy) (halves r))

let keeps_flags = function
  | ("ld" | "push" | "jp" | "out"), _ -> true
  | "pop", [ R r ] -> r <> "af"
  | ("inc" | "dec"), [ R r ] -> String.length r = 2
  | _ -> false

let push c op args = c.out <- (Ins (op, args), c.w) :: c.out

(* 構造体のフィールドを順に読むとき、ポインタを進めたままにしておく（ofs がその量）。
   次のフィールドへは差の分だけ進めばよい。ポインタを本来の値として使う命令の前で、まとめて戻す。
   HL は (hl) で何とでも読み書きできる。DE と BC は ld a,(de) / ld (de),a のように A とだけ。 *)
let ofs_of c r = Option.value ~default:0 (List.assoc_opt r c.ofs)
let set_ofs c r n = c.ofs <- (if n = 0 then [] else [ r, n ]) @ List.remove_assoc r c.ofs

let settle_r c r =
  let n = ofs_of c r in
  set_ofs c r 0;
  for _ = 1 to abs n do push c (if n > 0 then "dec" else "inc") [ R r ] done

let settle c = List.iter (fun (r, _) -> settle_r c r) c.ofs

let rec mentions r = function R x -> x = r || List.mem x (halves r) | M x -> mentions r x | _ -> false

(* 検査なしで出す。レジスタを最終的に変えない命令列や、フラグだけの命令に使う。 *)
let raw c op args =
  if c.ofs <> [] && not c.walking then
    List.iter
      (fun (r, _) ->
        match op, args with
        | "ret", [] -> set_ofs c r 0                    (* 戻るだけならポインタはもう要らない *)
        | "ret", _ -> ()                               (* 条件付き: 戻らなかったときは、進んだままで続ける *)
        | ("ld" | "pop"), R r' :: rest when r' = r && not (List.exists (mentions r) rest) -> set_ofs c r 0   (* 上書き *)
        | "call", _ when List.mem (hi r) c.busy -> ()  (* 呼んだ先はポインタを壊さない（壊すなら行き詰まっている） *)
        | ("jp" | "call" | "djnz"), _ -> if List.mem (hi r) c.busy then settle_r c r else set_ofs c r 0
        | _ -> if List.exists (mentions r) args then settle_r c r)
      c.ofs;
  push c op args

let label c l = settle c; c.out <- (Label l, c.w) :: c.out
let rem c s = c.out <- (Rem s, c.w) :: c.out

(* 命令を出す。生きている値を壊すなら Stuck。覚えているレジスタの中身とフラグも更新する。 *)
let emit c op args =
  let w = writes (op, args) in
  (try guard c w with Stuck why -> raise (Stuck (why ^ " ← " ^ show_ins (op, args))));
  if List.mem "a" w then c.ca <- None;
  if List.mem "h" w || List.mem "l" w then (c.chl <- None; c.hnear <- None);
  if not (keeps_flags (op, args)) then c.zf <- None;
  raw c op args

(* 先頭から n バイト分だけ取る *)
let rec first n = function
  | ((l, _) as x) :: rest when n > 0 -> x :: first (n - (match l with Ins i -> snd (Cost.measure i) | _ -> 0)) rest
  | _ -> []

let fresh c base = c.n <- c.n + 1; Printf.sprintf "%s.%s%d" c.f base c.n

let loc c t = function
  | C n -> Imm (Num (if t = U8 then n land 0xff else n land 0xffff))
  | S (s, n) ->
    let e = if n = 0 then Sym s else Add (Sym s, Num n) in
    Imm (if t = U8 then Lo e else e)
  | V x -> let h = c.home x in if is_mem h then Slot h else Reg h

(* 空いている作業用レジスタを選ぶ *)
let scratch c avoid cands =
  let ok r = free c r && not (List.exists (fun x -> List.mem x avoid) (halves r)) in
  match List.find_opt ok cands with Some r -> r | None -> raise (Stuck "作業用レジスタがない")

(* 値を移す。Z80 で直接は移せない組み合わせは、A か 16bit レジスタを経由する。 *)
let rec mv c t ?(avoid = []) dst src =
  match t, dst, src with
  | _, Imm _, _ -> assert false
  | _, Reg d, Reg s when d = s -> ()
  | _, Slot d, Slot s when d = s -> ()
  | U8, Reg d, Reg s -> emit c "ld" [ R d; R s ]
  | U8, Reg "a", Imm (Num 0) when c.zf = None -> emit c "xor" [ a ]   (* フラグを使う予定がなければ *)
  | U8, Reg d, Imm e -> emit c "ld" [ R d; I e ]
  | U8, Reg "a", Slot s -> emit c "ld" [ a; M (I (addr s)) ]
  | U8, Slot d, Reg "a" -> guard c [ d ]; emit c "ld" [ M (I (addr d)); a ]
  | U8, _, _ ->
    let r = scratch c avoid [ "a" ] in
    mv c t (Reg r) src;
    mv c t dst (Reg r)
  | U16, Reg d, Reg s when is_x d || is_x s -> emit c "push" [ R s ]; emit c "pop" [ R d ]
  | U16, Reg d, Reg s -> emit c "ld" [ R (hi d); R (hi s) ]; emit c "ld" [ R (lo d); R (lo s) ]
  | U16, Reg d, Imm e -> emit c "ld" [ R d; I e ]
  | U16, Reg d, Slot s -> emit c "ld" [ R d; M (I (addr s)) ]
  | U16, Slot d, Reg s -> guard c [ d ]; emit c "ld" [ M (I (addr d)); R s ]
  | U16, Slot _, _ ->
    let r = scratch c avoid [ "hl"; "de"; "bc" ] in
    mv c t (Reg r) src;
    mv c t dst (Reg r)

let to_a c v = if c.ca <> Some v then (mv c U8 (Reg "a") (loc c U8 v); c.ca <- Some v)
(* HL を「変数 p + n」にする。HL が p の近くを指していると分かっていれば、読み直さずに inc/dec で動かす。
   ポインタが DE や BC にあるとき、mem[p - 1] と mem[p] を続けて読み書きするのに使う。 *)
let near c p n =
  guard c [ "h"; "l" ];
  let cur = match c.hnear with Some (q, k) when q = p -> Some k | _ -> if c.chl = Some (V p) then Some 0 else None in
  let move d = for _ = 1 to abs d do push c (if d > 0 then "inc" else "dec") [ hl ] done in
  (match cur with
   | Some k when abs (n - k) <= 3 -> move (n - k)
   | _ -> mv c U16 (Reg "hl") (loc c U16 (V p)); move n);
  c.chl <- (if n = 0 then Some (V p) else None);
  c.hnear <- Some (p, n)

let to_hl c v =
  if c.chl <> Some v then begin
    match v, c.hnear with
    | V p, Some (q, _) when p = q && free c "hl" -> near c p 0
    | _ -> mv c U16 (Reg "hl") (loc c U16 v); c.chl <- Some v
  end

(* 計算結果（A か HL にある）を変数の置き場所へ *)
let st c t x =
  if c.last = Some x then c.held <- Some (x, t) else mv c t (loc c t (V x)) (Reg (if t = U8 then "a" else "hl"));
  if t = U8 then c.ca <- Some (V x) else c.chl <- Some (V x)

(* A との演算の相手として使える形にする。メモリにあるなら HL で指す。 *)
let opnd8 c v =
  match loc c U8 v with
  | Reg r -> R r
  | Imm e -> I e
  | Slot s -> emit c "ld" [ hl; I (addr s) ]; M hl

(* 値を持っている 16bit レジスタ（HL 以外）。なければ空いている組へ読み込む。 *)
let pair_of c v =
  match loc c U16 v with
  | Reg (("de" | "bc") as r) -> r
  | l ->
    let r = scratch c [] [ "de"; "bc" ] in
    mv c U16 (Reg r) l;
    r

let alu c op x = emit c op (if List.mem op [ "add"; "adc"; "sbc" ] then [ a; x ] else [ x ])
let op8 = function "+" -> "add" | "-" -> "sub" | "&" -> "and" | "|" -> "or" | _ -> "xor"
let in_a c v = match v with V x -> c.home x = "a" | _ -> false
let times n f = for _ = 1 to n do f () done

(* 比較してフラグを立て、成り立つときの条件名を返す。
   Z80 の比較は「等しい」と「小さい」しか直接は分からないので、> と <= は左右を入れ替える。 *)
let compare c o t x y =
  let x, y, cc =
    match o with
    | "==" -> x, y, "z"
    | "!=" -> x, y, "nz"
    | "<" -> x, y, "c"
    | ">=" -> x, y, "nc"
    | ">" -> y, x, "c"
    | _ -> y, x, "nc"
  in
  let eq = cc = "z" || cc = "nz" in
  let name = function V n -> Some n | _ -> None in
  (match t with
   | U8 when eq && (x = C 0 || y = C 0) ->
     (* 0 との比較。直前の演算のフラグが使えるなら何も出さない。 *)
     let v = if x = C 0 then y else x in
     if name v <> None && c.zf = name v then ()
     else begin
       (match loc c U8 v with
        | Reg r when r <> "a" && c.ca <> Some v -> raw c "inc" [ R r ]; raw c "dec" [ R r ]
        | _ -> to_a c v; raw c "or" [ a ]);
       c.zf <- name v
     end
   | U8 ->
     let x, y = if eq && in_a c y then y, x else x, y in
     if in_a c y && x <> y then raise (Stuck "比較の右が A");
     to_a c x;
     let o = opnd8 c y in
     emit c "cp" [ o ]
   | U16 when eq && (x = C 0 || y = C 0) ->
     let v = if x = C 0 then y else x in
     let r = match loc c U16 v with Reg r when c.chl <> Some v -> r | _ -> to_hl c v; "hl" in
     emit c "ld" [ a; R (hi r) ];
     emit c "or" [ R (lo r) ]
   | U16 when eq ->
     (* 引いて 0 かを見る。HL に生きた値があるなら、足し戻す（add hl は Z を変えない）。 *)
     let p = pair_of c y in
     to_hl c x;
     raw c "or" [ a ];
     raw c "sbc" [ hl; R p ];
     if free c "hl" then (c.chl <- None; c.hnear <- None) else raw c "add" [ hl; R p ];
     c.zf <- None
   | U16 ->
     (* 大小は下位から引いて繰り下がりを見る。A だけを使い、比べる値は壊さない。 *)
     let xr = match loc c U16 x with Reg r when c.chl <> Some x -> r | _ -> to_hl c x; "hl" in
     let ylo, yhi =
       match loc c U16 y with
       | Imm e -> I (Lo e), I (Hi e)
       | Reg r -> R (lo r), R (hi r)
       | Slot _ -> let p = pair_of c y in R (lo p), R (hi p)
     in
     emit c "ld" [ a; R (lo xr) ];
     emit c "sub" [ ylo ];
     emit c "ld" [ a; R (hi xr) ];
     emit c "sbc" [ a; yhi ]);
  cc

let neg_cc = function "z" -> "nz" | "nz" -> "z" | "c" -> "nc" | _ -> "c"

(* 小さな定数の足し引きは inc/dec で済ませる。+n なら n、-n なら -n を返す。 *)
let step t o v =
  let m = if t = U8 then 0x100 else 0x10000 in
  match v with
  | C n ->
    let n = (if o = "+" then n else -n) land (m - 1) in
    let n = if n > m / 2 then n - m else n in
    if abs n <= (if t = U8 then 1 else 3) && n <> 0 then Some n else None
  | _ -> None

let bump c r n = times (abs n) (fun () -> emit c (if n > 0 then "inc" else "dec") [ r ])

(* let x = e。準備（値をレジスタへ集める）の間は、これから読む値も壊せない。
   演算そのものと結果の格納では、この後も生きる値だけを守ればよい。 *)
let rec exp c x t e (before : SS.t) (after : SS.t) =
  set_live c (SS.union before after);
  let go () = set_live c after in
  let hx = c.home x in
  let done8 () = st c U8 x; c.zf <- Some x in
  match t, e with
  | _, Atom v -> go (); mv c t (loc c t (V x)) (loc c t v)
  (* ---- 8bit ---- *)
  | U8, Bin ((("+" | "-") as o), v, k) when step U8 o k <> None && not (is_mem hx) && hx <> "a"
                                           && (match loc c U8 v with Slot _ -> false | _ -> true) ->
    (* 置き場所のレジスタをその場で増減する。A を使わない。 *)
    go ();
    mv c U8 (Reg hx) (loc c U8 v);
    bump c (R hx) (Option.get (step U8 o k));
    c.zf <- Some x
  | U8, Bin ((("+" | "-") as o), V v, k) when step U8 o k <> None && is_mem hx && c.home v = hx ->
    (* メモリ上の変数をその場で増減する *)
    go ();
    guard c [ hx ];
    emit c "ld" [ hl; I (addr hx) ];
    bump c (M hl) (Option.get (step U8 o k));
    c.zf <- Some x
  | U8, Bin (o, v, w) ->
    let v, w = if o <> "-" && (in_a c w || c.ca = Some w) && not (in_a c v) then w, v else v, w in
    if in_a c w && v <> w then raise (Stuck "演算の右が A");
    to_a c v;
    (match (if o = "+" || o = "-" then step U8 o w else None) with
     | Some n -> go (); bump c a n
     | None -> let r = opnd8 c w in go (); alu c (op8 o) r);
    done8 ()
  | U8, Sh (o, v, n) ->
    to_a c v;
    go ();
    if o = "sar" then times (min n 7) (fun () -> emit c "sra" [ a ])
    else if n >= 8 then emit c "xor" [ a ]
    else times n (fun () -> if o = "<<" then emit c "add" [ a; a ] else emit c "srl" [ a ]);
    st c U8 x
  | _, Idx (s, v) ->
    (* 表の番地 + 添字。表はページをまたがないので、上位は決まっていて、下位だけ足せばよい。 *)
    let dst = if is_mem hx then "hl" else hx in
    (match v with
     | C n -> go (); emit c "ld" [ R dst; I (Add (Sym s, Num n)) ]
     | _ ->
       to_a c v;
       go ();
       emit c "add" [ a; I (Lo (Sym s)) ];
       emit c "ld" [ R (lo dst); a ];
       emit c "ld" [ R (hi dst); I (Hi (Sym s)) ]);
    if is_mem hx then st c U16 x
  | _, Lut (s, v) ->
    (* 256 バイト境界に置いた表: 添字を L に、表のページを H に入れるだけで番地になる *)
    let idx = match loc c U8 v with Reg r -> R r | Imm e -> I e | Slot _ -> to_a c v; a in
    go ();
    emit c "ld" [ R "l"; idx ];
    emit c "ld" [ R "h"; I (Hi (Sym s)) ];
    if t = U8 then begin
      if is_mem hx then (emit c "ld" [ a; M hl ]; st c U8 x) else emit c "ld" [ R hx; M hl ]
    end
    else if hx = "de" || hx = "bc" then begin          (* 上位の表はすぐ次のページ *)
      emit c "ld" [ R (lo hx); M hl ]; emit c "inc" [ R "h" ]; emit c "ld" [ R (hi hx); M hl ]
    end else begin
      emit c "ld" [ a; M hl ]; emit c "inc" [ R "h" ]; emit c "ld" [ R "h"; M hl ]; emit c "ld" [ R "l"; a ];
      st c U16 x
    end
  | _, Mulc (v, n) ->
    (* 定数倍: 上の桁から「2 倍して、桁が 1 なら元の値を足す」を繰り返す *)
    let bits = List.filter (fun b -> n lsr b > 0) [ 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0 ] in
    if t = U8 then to_a c v else to_hl c v;
    let acc = if t = U8 then a else hl in
    let copy =
      if List.length (List.filter (fun b -> n lsr b land 1 = 1) bits) < 2 then None
      else if t = U8 then (let r = scratch c [ "a" ] [ "b"; "c"; "d"; "e"; "h"; "l" ] in Some (R r))
      else Some (R (scratch c [] [ "de"; "bc" ]))
    in
    go ();
    (match copy with
     | Some (R r) when t = U8 -> emit c "ld" [ R r; a ]
     | Some (R r) -> emit c "ld" [ R (hi r); R "h" ]; emit c "ld" [ R (lo r); R "l" ]
     | _ -> ());
    if n = 0 then (if t = U8 then emit c "xor" [ a ] else emit c "ld" [ hl; I (Num 0) ])
    else
      List.iter
        (fun b ->
          emit c "add" [ acc; acc ];
          if n lsr b land 1 = 1 then emit c "add" [ acc; Option.get copy ])
        (List.tl bits);
    st c t x
  | U8, Un (o, v) -> to_a c v; go (); emit c (if o = "-" then "neg" else "cpl") []; st c U8 x
  | U8, Trunc v ->
    (* 下位バイトだけ取る。メモリでは下位が先に置いてある。 *)
    go ();
    let src = match loc c U16 v with
      | Reg r -> Reg (lo r)
      | Imm e -> Imm (Lo e)
      | Slot s -> if c.chl = Some v then Reg "l" else Slot s
    in
    mv c U8 (loc c U8 (V x)) src
  | _, Cmp (o, t', v, w) ->
    let cc = compare c o t' v w in
    let l = fresh c "c" in
    go ();
    emit c "ld" [ a; I (Num 1) ];                     (* ld はフラグを変えない *)
    raw c "jp" [ R cc; I (Sym l) ];
    emit c "dec" [ a ];
    label c l;
    c.ca <- None;
    st c U8 x
  | U8, Load p ->
    let m = match loc c U16 p with
      | Imm e -> M (I e)
      | Reg r when c.chl <> Some p -> M (R r)
      | _ -> to_hl c p; M hl
    in
    go ();
    if m = M hl && not (is_mem hx) then emit c "ld" [ R hx; m ]
    else (emit c "ld" [ a; m ]; st c U8 x)
  | _, In p -> go (); emit c "in" [ a; M (I (Num p)) ]; st c U8 x
  (* ---- 16bit ---- *)
  | U16, Bin ((("+" | "-") as o), v, k) when step U16 o k <> None ->
    let n = Option.get (step U16 o k) in
    if is_mem hx then (to_hl c v; go (); bump c hl n; st c U16 x)
    else begin
      go ();
      let was = c.hnear in
      mv c U16 (Reg hx) (loc c U16 v);
      bump c (R hx) n;
      (* HL が v の近くを指していたなら、x から見た位置に言い直す *)
      match v, was with
      | V u, Some (u', k) when u = u' && c.hnear = was -> c.hnear <- Some (x, k - n)
      | _ -> ()
    end
  | U16, Bin ("+", V p, C n) when is_x hx && c.home p = hx ->
    (* IX/IY をその場で進める（構造体の配列を歩くとき）。HL を通さない *)
    go ();
    let r = scratch c [] [ "de"; "bc" ] in
    emit c "ld" [ R r; I (Num n) ];
    emit c "add" [ R hx; R r ]
  | U16, Bin ("-", v, C n) -> exp_add c x v (C (-n land 0xffff)) go
  | U16, Bin ("+", v, w) -> exp_add c x v w go
  | U16, Bin ("-", v, w) ->
    let p = pair_of c w in
    to_hl c v;
    go ();
    emit c "or" [ a ];
    emit c "sbc" [ hl; R p ];
    st c U16 x
  | U16, Bin (o, v, w) ->
    (* and/or/xor は 1 バイトずつ A を通す *)
    let wl, wh = match loc c U16 w with
      | Imm e -> I (Lo e), I (Hi e)
      | Reg r when r <> "hl" -> R (lo r), R (hi r)
      | _ -> let p = pair_of c w in R (lo p), R (hi p)
    in
    to_hl c v;
    go ();
    List.iter
      (fun (h, w) -> emit c "ld" [ a; R h ]; alu c (op8 o) w; emit c "ld" [ R h; a ])
      [ "h", wh; "l", wl ];
    st c U16 x
  | U16, Sh (o, v, n) ->
    to_hl c v;
    go ();
    let l = R "l" and h = R "h" in
    if o = "sar" then begin
      if n >= 8 then begin                            (* 上位を下位へ写し、上位は符号で埋める *)
        emit c "ld" [ l; h ]; emit c "ld" [ a; h ]; emit c "rla" []; emit c "sbc" [ a; a ]; emit c "ld" [ h; a ];
        times (min (n - 8) 7) (fun () -> emit c "sra" [ l ])
      end else times n (fun () -> emit c "sra" [ h ]; emit c "rr" [ l ])
    end
    else if n >= 16 then emit c "ld" [ hl; I (Num 0) ]
    else if o = "<<" && n = 7 then begin
      (* 7 回の add hl,hl（77T）の代わりに、8 ずらして 1 戻す（35T）。h の最下位は桁上げで運ぶ *)
      emit c "rr" [ h ]; emit c "ld" [ h; l ]; emit c "rr" [ h ];
      emit c "ld" [ l; I (Num 0) ]; emit c "rr" [ l ]
    end
    else if o = "<<" then begin
      if n >= 8 then (emit c "ld" [ h; l ]; emit c "ld" [ l; I (Num 0) ]);
      times (n land 7) (fun () -> emit c "add" [ hl; hl ])
    end else begin
      if n >= 8 then (emit c "ld" [ l; h ]; emit c "ld" [ h; I (Num 0) ]);
      times (n land 7) (fun () -> if n >= 8 then emit c "srl" [ l ] else (emit c "srl" [ h ]; emit c "rr" [ l ]))
    end;
    st c U16 x
  | U16, Un (o, v) ->
    to_hl c v;
    go ();
    if o = "-" then begin                             (* 0 - HL を A で 1 バイトずつ *)
      emit c "xor" [ a ]; emit c "sub" [ R "l" ]; emit c "ld" [ R "l"; a ];
      emit c "sbc" [ a; a ]; emit c "sub" [ R "h" ]; emit c "ld" [ R "h"; a ]
    end else
      List.iter (fun h -> emit c "ld" [ a; R h ]; emit c "cpl" []; emit c "ld" [ R h; a ]) [ "h"; "l" ];
    st c U16 x
  | _, Zext v ->
    let dst = if is_mem hx then "hl" else hx in
    let src = match loc c U8 v with
      | Reg r -> R r
      | Imm e -> I e
      | Slot _ -> to_a c v; a
    in
    go ();
    emit c "ld" [ R (lo dst); src ];
    emit c "ld" [ R (hi dst); I (Num 0) ];
    if is_mem hx then st c U16 x
  | U16, Load p ->
    (match loc c U16 p, hx with
     | Reg (("ix" | "iy") as r), ("de" | "bc" | "hl") ->
       go ();
       emit c "ld" [ R (lo hx); M (X (r, Num 0)) ];
       emit c "ld" [ R (hi hx); M (X (r, Num 1)) ]
     | Imm e, _ ->
       go ();
       if is_mem hx then (emit c "ld" [ hl; M (I e) ]; st c U16 x) else emit c "ld" [ R hx; M (I e) ]
     | _, ("de" | "bc") ->
       (* 結果を直接その組へ。HL のポインタは動かして戻すので、変わらない。 *)
       to_hl c p;
       go ();
       emit c "ld" [ R (lo hx); M hl ];
       raw c "inc" [ hl ];
       emit c "ld" [ R (hi hx); M hl ];
       raw c "dec" [ hl ]
     | _ ->
       to_hl c p;
       go ();
       emit c "ld" [ a; M hl ]; emit c "inc" [ hl ]; emit c "ld" [ R "h"; M hl ]; emit c "ld" [ R "l"; a ];
       st c U16 x)
  | U16, Trunc _ -> assert false

and exp_add c x v w go =
  (* 片方が DE/BC にあり、もう片方が定数かメモリで、作業用の組が空いていないなら、そちらを HL へ読む。
     空いているときにも使うと、探索の値段が変わって、別の割り当て（悪いほう）に落ちた（damaxian の BulletMove）。
     空いていないときだけなら、前は行き詰まった割り当てが通るようになるだけ（ループの回数を B に置ける） *)
  let in_pair v = match loc c U16 v with Reg ("de" | "bc") -> c.chl <> Some v | _ -> false in
  let v, w =
    if (match w with V y -> c.home y = "hl" | _ -> false) || c.chl = Some w then w, v
    else if in_pair v && (match loc c U16 w with Reg _ -> false | _ -> true)
            && not (List.exists (free c) [ "de"; "bc" ]) then w, v
    else v, w
  in
  let p = if v = w then "hl" else pair_of c w in
  to_hl c v;
  go ();
  emit c "add" [ hl; R p ];
  st c U16 x

(* いくつかの置き場所へ同時に値を配る（継続へ飛ぶとき、関数を呼ぶとき）。
   後で読む場所を先に上書きしないよう順番を決める。輪になっていたら、1 つを作業用レジスタへ逃がす。
   ms: (行き先, 型, 元, 行き先の変数名) *)
let pmove c live ms =
  let place = function Reg r -> halves r | Slot h -> [ h ] | Imm _ -> [] in
  let srcs ms = List.concat_map (fun (_, _, s, _) -> place s) ms in
  let dsts ms = List.concat_map (fun (d, _, _, _) -> place d) ms in
  let rec go ms done_ =
    if ms <> [] then begin
      let others m = List.filter (( != ) m) ms in
      let ready ((d, _, _, _) as m) =
        not (List.exists (fun r -> List.mem r (srcs (others m))) (place d))
      in
      (* 経由レジスタ（A など）が要る移動を先に、A へ入れる移動を最後にする。
         先に A を埋めると、経由に使えなくなる。 *)
      let rank (d, t, s, _) =
        match t, d, s with
        | _, Reg "a", _ -> 2
        | U8, Reg _, (Reg _ | Imm _) | U8, Slot _, Reg "a" | U16, Reg _, _ | U16, Slot _, Reg _ -> 1
        | _ -> 0
      in
      let first = List.sort (fun x y -> Stdlib.compare (rank x) (rank y)) (List.filter ready ms) in
      match (match first with m :: _ -> Some m | [] -> None) with
      | Some ((d, t, s, x) as m) ->
        set_live c (match x with Some x -> SS.remove x live | None -> live);
        mv c t ~avoid:(srcs (others m) @ done_) d s;
        go (others m) (place d @ done_)
      | None ->
        let d, t, s, x = List.hd ms in
        let taken = srcs ms @ dsts ms @ done_ in
        set_live c live;
        let r = scratch c taken (if t = U8 then [ "b"; "c"; "d"; "e"; "h"; "l"; "a" ] else [ "de"; "bc"; "hl" ]) in
        mv c t ~avoid:taken (Reg r) s;
        go ((d, t, Reg r, x) :: List.tl ms) done_
    end
  in
  (* すでに行き先にある値は動かさない。ただし、その場所を経由に使ってはいけない。 *)
  let same, rest = List.partition (fun (d, _, s, _) -> d = s) ms in
  go rest (dsts same)

(* 配る値のありか。行き先が A（HL）で、値がもうそこに入っていると分かっていれば、動かさなくてよい *)
let src c d t v =
  if (d = Reg "a" && t = U8 && c.ca = Some v) || (d = Reg "hl" && t = U16 && c.chl = Some v) then d else loc c t v

let klabel c k = I (Sym (c.f ^ "." ^ k))

(* 何も移さずに飛べるか *)
let pure c k args =
  let ps, _ = Hashtbl.find c.konts k in
  List.for_all2 (fun (p, t) v -> loc c t (V p) = loc c t v) ps args

(* 条件付きで継続へ飛ぶ。飛んだ先で HL の値が要らないなら、HL は進んだままでよい（戻す命令を出さない） *)
let jcc c cc k =
  let ps, live = Hashtbl.find c.konts k in
  let dead (r, _) =
    let inr x = List.exists (fun h -> List.mem h (halves r)) (places (c.home x)) in
    not (SS.exists inr live || List.exists (fun (p, _) -> inr p) ps)
  in
  if c.ofs <> [] && List.for_all dead c.ofs
  then push c "jp" [ R cc; klabel c k ]
  else raw c "jp" [ R cc; klabel c k ]

let show_home h = if is_mem h then "mem" else String.uppercase_ascii h
let show_params c ps = String.concat ", " (List.map (fun (p, _) -> p ^ ": " ^ show_home (c.home p)) ps)

let reset c = c.ca <- None; c.chl <- None; c.hnear <- None; c.zf <- None; c.held <- None

(* ポインタ r（既定は HL）を本来の値 + n の位置へ動かす。以後、walking を下ろすまでの命令は進んだままの r を使う。 *)
let walk ?(r = "hl") c n =
  let d = n - ofs_of c r in
  for _ = 1 to abs d do push c (if d > 0 then "inc" else "dec") [ R r ] done;
  set_ofs c r n;
  if r = "hl" then (c.chl <- None; c.hnear <- None);
  c.walking <- true

(* ---- 番地 + 定数にあるフィールド。構造体と、プールのタスクのフレームで使う ---- *)

(* 定数の足し引きなら、その量 *)
let disp o = function
  | C n -> let n = (if o = "+" then n else -n) land 0xffff in Some (if n > 0x7fff then n - 0x10000 else n)
  | _ -> None

let fieldable c p = List.mem (c.home p) [ "hl"; "de"; "bc" ] || is_x (c.home p)

(* p + n を指すオペランド。p が HL なら HL をそこまで進めて (hl)、IX なら (ix+n)。
   DE, BC なら、A と読み書きするとき（a）だけ、そこまで進めて (de)。 *)
let field ?(a = false) c p n =
  match c.home p with
  | "hl" when abs (n - ofs_of c "hl") <= 4 -> walk c n; M hl
  | ("de" | "bc") as r when a && abs (n - ofs_of c r) <= 4 -> walk ~r c n; M (R r)
  | ("ix" | "iy") as x when n >= -128 && n < 127 -> M (X (x, Num n))
  | _ -> raise (Stuck "フィールドを直接は指せない")

(* 続く 1 バイト（16bit の上位） *)
let field2 c p n =
  if is_x (c.home p) then M (X (c.home p, Num (n + 1)))
  else if c.home p = "hl" then (push c "inc" [ hl ]; set_ofs c "hl" (ofs_of c "hl" + 1); M hl)
  else raise (Stuck "16bit は DE, BC を歩いて読み書きしない")

(* タスクの「続きの番地」を書く。None は「終わった」。
   1 個だけのタスクは決まった場所に書く。プールのタスクは、フレームの変数の手前 2 バイトに書く。
   プールでは上位バイトが 0 なら「空き」。 *)
let set_pc c l =
  match c.self, l with
  | None, l ->
    let l = Option.value ~default:(c.f ^ ".done") l in
    emit c "ld" [ hl; I (Sym l) ];
    emit c "ld" [ M (I (Sym (c.f ^ ".pc"))); hl ]
  | Some s, l ->
    set_live c (SS.singleton s);
    if not (c.home s = "hl" || is_x (c.home s)) then to_hl c (V s);
    let at n = if is_x (c.home s) then M (X (c.home s, Num n)) else (walk c n; M hl) in
    (match l with
     | Some l ->
       let m = at (-1) in emit c "ld" [ m; I (Hi (Sym l)) ];
       let m = at (-2) in emit c "ld" [ m; I (Lo (Sym l)) ]
     | None -> let m = at (-1) in emit c "ld" [ m; I (Num 0) ]);
    c.walking <- false

(* 入口と再開の直後: フレームの番地は HL で届く。self の置き場所へ移す。 *)
let enter c =
  match c.self with
  | Some s -> set_live c SS.empty; mv c U16 (loc c U16 (V s)) (Reg "hl"); c.chl <- Some (V s)
  | None -> ()

(* ---- 値での分岐（match）。出し方は 3 通りあり、term が全部出してみて安いものを選ぶ ---- *)

let only : string option ref = ref None      (* 計測用: 出し方を "lin" "bin" "tab" "tab2" のどれかに絞る *)

(* 比べて分ける。値は A にあり、lo..hi のどれかだと分かっている。
   枝が leaf 個以下なら順に比べ、多ければ真ん中の値で 2 つに割る。leaf が大きければ全部を順に比べる。
   dec なら A を減らしながら比べる（続き番号なら dec a で済み、cp n より速い）。A は「値 - off」。
   どの枝も同じ頻度で来るとみなして、命令ごとに「ここを通る割合」を重みにする。unit が枝 1 つ分。 *)
let rec tree c v other ~leaf ~dec unit cs lo hi off =
  let n = List.length cs in
  c.w <- unit *. float (n + if other = None then 0 else 1);
  match cs with
  | [] -> raw c "jp" [ klabel c (Option.get other) ]
  | (_, k) :: rest when List.for_all (fun (_, k') -> k' = k) rest && (other = None || n = hi - lo + 1) ->
    raw c "jp" [ klabel c k ]                    (* 残りは全部同じ行き先。比べなくてよい *)
  | (x, k) :: rest when n <= leaf ->
    (match dec, (x - off) land 255 with
     | false, _ | _, 0 -> ignore (compare c "==" U8 v (C x))
     | _, 1 -> emit c "dec" [ a ]
     | _, 255 -> emit c "inc" [ a ]
     | _, d -> emit c "sub" [ I (Num d) ]);
    raw c "jp" [ R "z"; klabel c k ];
    c.zf <- None;
    tree c v other ~leaf ~dec unit rest (if x = lo then lo + 1 else lo) (if x = hi then hi - 1 else hi)
      (if dec then x else 0)
  | _ ->
    let low = List.filteri (fun i _ -> i < n / 2) cs and high = List.filteri (fun i _ -> i >= n / 2) cs in
    let m = fst (List.hd high) and l = fresh c "lt" and ca = c.ca in
    emit c "cp" [ I (Num m) ];
    raw c "jp" [ R "c"; I (Sym l) ];
    tree c v other ~leaf ~dec unit high m hi 0;
    label c l;
    c.ca <- ca;
    tree c v other ~leaf ~dec unit low lo (m - 1) 0

(* 表で飛ぶ。256 バイト境界に置いた表の前半に飛び先の下位、128 バイト先に上位を並べる。
   値をそのまま L に入れれば下位が、bit7 を立てれば上位が読める。
   2 ページ版（v2）は下位と上位を別ページに並べ、上位は inc h で読む。set 7,l の 8 T が
   inc h の 4 T になり、飛ぶまで 41 T が 37 T になる。代わりに表は 2 ページ分になり、
   値の上限も 127 から 255 に届く。2 つ目の Align 256 は、すでに境界上にあるので
   アセンブラは何も出さないが、値段の計算では表の大きさとして数えられる。 *)
let table ~v2 c other cs =
  let top = List.fold_left (fun m (n, _) -> max m n) 0 cs in
  if top >= (if v2 then 256 else 128) then raise (Stuck "値が大きくて表にできない");
  (match other with
   | Some d when not (v2 && top = 255) ->
     (* 値が 255 まで全部表に入っているときは、範囲の確認は要らない *)
     emit c "cp" [ I (Num (top + 1)) ]; raw c "jp" [ R "nc"; klabel c d ]
   | _ -> ());
  let tbl =
    match List.find_opt (fun (cs', other', v2', _) -> cs' = cs && other' = other && v2' = v2) c.tabs with
    | Some (_, _, _, tbl) -> tbl                      (* 同じ分岐の複製なら、表は 1 つでよい *)
    | None ->
      let tbl = fresh c "table" in
      let fill = match other with Some d -> d | None -> snd (List.hd cs) in
      let entry i = Sym (c.f ^ "." ^ match List.assoc_opt i cs with Some k -> k | None -> fill) in
      let column f = Db (List.init (top + 1) (fun i -> f (entry i))) in
      c.data <- c.data @ (if v2 then
        [ Align 256; Label tbl; column (fun e -> Lo e); Ds (255 - top); Align 256; column (fun e -> Hi e) ]
      else
        [ Align 256; Label tbl; column (fun e -> Lo e); Ds (127 - top); column (fun e -> Hi e) ]);
      c.tabs <- (cs, other, v2, tbl) :: c.tabs;
      tbl
  in
  emit c "ld" [ R "l"; a ];
  emit c "ld" [ R "h"; I (Hi (Sym tbl)) ];
  emit c "ld" [ a; M hl ];
  if v2 then emit c "inc" [ R "h" ] else emit c "set" [ I (Num 7); R "l" ];
  emit c "ld" [ R "h"; M hl ];
  emit c "ld" [ R "l"; a ];
  raw c "jp" [ M hl ]

let rec term c (t : L.t) =
  match t.n with
  | L.Let (x, _, e, k) when (not (SS.mem x k.lv)) && (match e with Load _ | In _ -> false | _ -> true) ->
    term c k                                          (* 使われない計算は出さない *)
  | L.Let (m, U8, Load (V p), { n = L.Let (x, U8, Bin (o, v, w), k); _ })
    when (not (SS.mem m k.lv)) && ((w = V m && v <> V m) || (v = V m && w <> V m && o <> "-")) ->
    (* メモリから読んですぐ演算に使うなら、add a,(hl) のように 1 命令にまとめる *)
    let other = if w = V m then v else w in
    let after = SS.remove x k.lv in
    attempt c
      (fun () ->
        set_live c (SS.union t.lv after);
        to_hl c (V p);
        to_a c other;
        set_live c after;
        alu c (op8 o) (M hl);
        st c U8 x;
        c.zf <- Some x;
        term c k)
      (fun () ->
        match t.n with
        | L.Let (_, _, e, k1) -> exp c m U8 e t.lv (SS.remove m k1.lv); term c k1
        | _ -> assert false)
  | L.Let (m, U16, (Bin ((("+" | "-") as o), V p, d) as e), ({ n = L.Let (x, ty, Load (V m'), k); _ } as inner))
    when m' = m && (not (SS.mem m k.lv)) && fieldable c p && disp o d <> None ->
    (* p.f を読む: p が HL なら f の位置まで進めて読む（HL は進めたままにしておく）。IX なら (ix+d)。 *)
    attempt c
      (fun () ->
        let hx = c.home x and n = Option.get (disp o d) and viahl = c.home p = "hl" in
        let read () =
          set_live c (SS.remove x k.lv);
          (match ty with
           | U8 when is_mem hx -> let f = field ~a:true c p n in emit c "ld" [ a; f ]; c.walking <- false; st c U8 x
           | U8 when viahl && (hx = "h" || hx = "l") -> raise (Stuck "HL を歩きながら H, L には読めない")
           | U8 -> let f = field ~a:(hx = "a") c p n in emit c "ld" [ R hx; f ]
           | U16 when hx = "de" || hx = "bc" || (hx = "hl" && not viahl) ->
             let f = field c p n in emit c "ld" [ R (lo hx); f ];
             let f = field2 c p n in emit c "ld" [ R (hi hx); f ]
           | U16 -> raise (Stuck "16bit のフィールドは DE か BC に読む"));
          c.walking <- false;
          term c k
        in
        match ty, k.n with
        | U8, L.Let (y, U8, Bin (op, v, w), k2)
          when (not (SS.mem x k2.lv)) && ((w = V x && v <> V x) || (v = V x && w <> V x && op <> "-"))
               && step U8 op (if w = V x then v else w) = None ->                (* ±1 は読んでから inc/dec の方が安い *)
          (* 読んですぐ演算に使うなら、add a,(hl) や add a,(ix+d) の 1 命令にまとめる *)
          attempt c
            (fun () ->
              let after = SS.remove y k2.lv in
              set_live c (SS.union t.lv after);
              to_a c (if w = V x then v else w);
              set_live c after;
              let f = field c p n in
              alu c (op8 op) f;
              c.walking <- false;
              st c U8 y;
              c.zf <- Some y;
              term c k2)
            read
        | _ -> read ())
      (fun () -> exp c m U16 e t.lv (SS.remove m inner.lv); term c inner)
  | L.Let (m, U16, (Bin ((("+" | "-") as o), V p, d) as e), ({ n = L.Store (ty, V m', v, k); _ } as inner))
    when m' = m && v <> V m && (not (SS.mem m k.lv)) && fieldable c p && disp o d <> None ->
    (* p.f へ書く *)
    attempt c
      (fun () ->
        let n = Option.get (disp o d) and viahl = c.home p = "hl" in
        set_live c (SS.remove m inner.lv);
        (match ty with
         | U8 ->
           let src = match loc c U8 v with
             | Reg ("h" | "l") when viahl -> raise (Stuck "H, L は書けない")
             | Reg r -> R r
             | Imm e -> I e
             | Slot _ -> to_a c v; a
           in
           let f = field ~a:(src = a) c p n in emit c "ld" [ f; src ]
         | U16 ->
           let wl, wh = match loc c U16 v with
             | Imm e -> I (Lo e), I (Hi e)
             | Reg ("de" | "bc") when c.chl = Some v && not viahl -> R "l", R "h"   (* HL にも同じ値がある *)
             | Reg (("de" | "bc") as r) -> R (lo r), R (hi r)
             | Reg "hl" when not viahl -> R "l", R "h"
             | _ -> raise (Stuck "16bit のフィールドへは DE か BC か定数から書く")
           in
           let f = field c p n in emit c "ld" [ f; wl ];
           let f = field2 c p n in emit c "ld" [ f; wh ]);
        c.walking <- false;
        term c k)
      (fun () -> exp c m U16 e t.lv (SS.remove m inner.lv); term c inner)
  | L.Let (m, U16, (Bin ((("+" | "-") as o), V p, d) as e), ({ n = L.Let (x, U8, Load (V m'), k); _ } as inner))
    when m' = m && (not (SS.mem m k.lv)) && c.pick && (match disp o d with Some n -> abs n <= 3 | None -> false) ->
    (* ポインタ p が HL 以外にあるときの mem[p ± n] の読み: HL を p ± n にして読む *)
    attempt c
      (fun () ->
        let hx = c.home x in
        set_live c (SS.remove x k.lv);
        near c p (Option.get (disp o d));
        if is_mem hx then (emit c "ld" [ a; M hl ]; st c U8 x) else emit c "ld" [ R hx; M hl ];
        term c k)
      (fun () -> exp c m U16 e t.lv (SS.remove m inner.lv); term c inner)
  | L.Let (m, U16, (Bin ((("+" | "-") as o), V p, d) as e), ({ n = L.Store (U8, V m', v, k); _ } as inner))
    when m' = m && v <> V m && (not (SS.mem m k.lv)) && c.pick && (match disp o d with Some n -> abs n <= 3 | None -> false) ->
    (* 同じく書き込み *)
    attempt c
      (fun () ->
        set_live c (SS.remove m inner.lv);
        let src = match loc c U8 v with
          | Reg ("h" | "l") -> raise (Stuck "H, L は書けない")
          | Reg r -> R r
          | Imm e -> I e
          | Slot _ -> to_a c v; a
        in
        near c p (Option.get (disp o d));
        emit c "ld" [ M hl; src ];
        term c k)
      (fun () -> exp c m U16 e t.lv (SS.remove m inner.lv); term c inner)
  | L.Let (m, U16, (Sh (">>", V v, 8) as e), ({ n = L.Let (x, U8, Trunc (V m'), k); _ } as inner))
    when m' = m && (not (SS.mem m k.lv)) ->
    (* (v >> 8) as u8 は、v の上位バイトを読むだけ *)
    attempt c
      (fun () ->
        let after = SS.remove x k.lv in
        set_live c (SS.union t.lv after);
        let src = loc c U16 (V v) in
        set_live c after;
        (match src with
         | Reg r -> mv c U8 (loc c U8 (V x)) (Reg (hi r))
         | Slot s -> emit c "ld" [ a; M (I (Add (addr s, Num 1))) ]; st c U8 x
         | Imm _ -> raise (Stuck "定数"));
        term c k)
      (fun () -> exp c m U16 e t.lv (SS.remove m inner.lv); term c inner)
  | L.Let (x, ty, e, k) ->
    (* 次の行で返すか、関数に渡して終わりの値なら、置き場所へ書くのは無駄 *)
    let brief =
      match k.n with
      | L.Ret (Some (V y, _)) -> y = x
      | L.Call (r, _, args, k2) ->
        List.exists (fun (v, _) -> v = V x) args
        && not (SS.mem x (match r with Some (y, _) -> SS.remove y k2.lv | None -> k2.lv))
      | _ -> false
    in
    if brief then c.last <- Some x;
    Fun.protect ~finally:(fun () -> c.last <- None) (fun () -> exp c x ty e t.lv (SS.remove x k.lv));
    term c k
  | L.Store (U8, p, v, k) ->
    set_live c t.lv;
    (match loc c U16 p with
     | Imm e -> to_a c v; emit c "ld" [ M (I e); a ]
     | Reg (("de" | "bc") as r) -> to_a c v; emit c "ld" [ M (R r); a ]
     | Reg (("ix" | "iy") as r) ->
       let src = match loc c U8 v with Reg r8 -> R r8 | Imm e -> I e | Slot _ -> to_a c v; a in
       emit c "ld" [ M (X (r, Num 0)); src ]
     | _ ->
       to_hl c p;
       (match loc c U8 v with
        | Reg r -> emit c "ld" [ M hl; R r ]
        | Imm e -> emit c "ld" [ M hl; I e ]
        | Slot _ -> to_a c v; emit c "ld" [ M hl; a ]));
    term c k
  | L.Store (U16, p, v, k) ->
    set_live c t.lv;
    (match loc c U16 p, loc c U16 v with
     | Imm e, Reg r -> emit c "ld" [ M (I e); R r ]
     | Imm e, _ -> to_hl c v; emit c "ld" [ M (I e); hl ]
     | Reg (("de" | "bc") as r), Reg "hl" ->
       (* 値が HL にあるので、ポインタは DE/BC のまま A 経由で書く *)
       emit c "ld" [ a; R "l" ]; emit c "ld" [ M (R r); a ];
       raw c "inc" [ R r ];
       emit c "ld" [ a; R "h" ]; emit c "ld" [ M (R r); a ];
       raw c "dec" [ R r ]
     | Reg (("ix" | "iy") as r), (Imm _ | Reg ("hl" | "de" | "bc") as lv) ->
       (* IX/IY が指す所へは、HL を通さずに (ix+0) と (ix+1) へ書く *)
       let wl, wh = match lv with Imm e -> I (Lo e), I (Hi e) | Reg q -> R (lo q), R (hi q) | _ -> assert false in
       emit c "ld" [ M (X (r, Num 0)); wl ];
       emit c "ld" [ M (X (r, Num 1)); wh ]
     | _, lv ->
       let wl, wh = match lv with
         | Imm e -> I (Lo e), I (Hi e)
         | _ -> let q = pair_of c v in R (lo q), R (hi q)
       in
       to_hl c p;
       emit c "ld" [ M hl; wl ];
       raw c "inc" [ hl ];
       emit c "ld" [ M hl; wh ];
       raw c "dec" [ hl ]);
    term c k
  | L.Out (p, v, k) -> set_live c t.lv; to_a c v; emit c "out" [ M (I (Num p)); a ]; term c k
  | L.Call (r, f, args, k) ->
    let after = match r with Some (x, _) -> SS.remove x k.lv | None -> k.lv in
    let user = Hashtbl.mem c.info f || List.mem_assoc f c.exts in      (* 同じ名前の関数を自分で定義していれば、そちら *)
    if List.mem f bare && not user then (raw c f []; term c k) else
    let prim = if user then None else List.assoc_opt f prims in      (* 引数をレジスタで受ける組み込み（ldir など） *)
    let dsts, clob, target =
      match List.assoc_opt f c.exts, prim with
      | _, Some (regs, clob, _) -> List.map (fun r -> Reg r) regs, clob, I (Num 0)
      | Some (regs, _, addr, clob), _ -> List.map (fun (_, r) -> Reg r) regs, clob, I (Num addr)
      | None, None ->
        let i = Hashtbl.find c.info f in
        List.map (fun h -> if is_mem h then Slot h else Reg h) i.phome, i.clob, I (Sym f)
    in
    let tail =                                        (* 呼んだ結果をそのまま返すだけか *)
      match r, k.n with
      | _ when prim <> None -> false
      | None, L.Ret None -> not c.task                (* タスクは終わりの印を付けてから戻る *)
      | Some (x, _), L.Ret (Some (V y, _)) -> x = y
      | _ -> false
    in
    let live = if tail then SS.empty else after in
    let plain () = pmove c live (List.map2 (fun d (v, t) -> d, t, loc c t v, None) dsts args) in
    let cached () = pmove c live (List.map2 (fun d (v, t) -> d, t, src c d t v, None) dsts args) in
    (* 置き場所へ書かずに持っている値は、A（HL）で渡すならそのまま。そうでなければ、ここで書いてから配る *)
    let flush () =
      match c.held with
      | Some (x, ty) ->
        c.held <- None;
        set_live c (SS.remove x t.lv);
        mv c ty (loc c ty (V x)) (Reg (if ty = U8 then "a" else "hl"))
      | None -> ()
    in
    (match c.held with
     | Some (x, _) when List.for_all2 (fun d (v, _) -> v <> V x || d = Reg "a" || d = Reg "hl") dsts args ->
       attempt c cached (fun () -> flush (); plain ())
     | Some _ -> flush (); plain ()
     | None -> attempt c cached plain);
    c.held <- None;
    if tail then raw c "jp" [ target ]               (* 末尾呼び出し: 戻り番地を積まずに飛ぶ *)
    else begin
    set_live c after;
    (* 壊されるレジスタに生きた値があれば、呼ぶ前に push し、戻ったら pop する（メモリへ退避するより安い）。
       戻り値が入る A（HL）の組は、pop で壊れるので使えない。 *)
    let pair h =
      match h with
      | "a" -> Some "af" | "b" | "c" | "bc" -> Some "bc" | "d" | "e" | "de" -> Some "de"
      | "h" | "l" | "hl" -> Some "hl" | "ix" | "iy" -> Some h | _ -> None
    in
    let ret_pair = match r with Some (x, t) when SS.mem x k.lv -> Some (if t = U8 then "af" else "hl") | _ -> None in
    let saves =
      SS.fold
        (fun x l ->
          let h = c.home x in
          match pair h with
          | Some q when List.exists (fun y -> List.mem y clob) (halves h) && not (List.mem q l) -> q :: l
          | _ -> l)
        after []
    in
    if List.exists (fun q -> Some q = ret_pair) saves then raise (Stuck "戻り値の組に生きた値がある");
    guard c (List.filter (fun y -> not (List.exists (fun q -> List.mem y (if q = "af" then [ "a" ] else halves q)) saves)) clob);
    List.iter (fun q -> raw c "push" [ R q ]) saves;
    (match prim with Some (_, _, (op, args)) -> raw c op args | None -> raw c "call" [ target ]);
    List.iter (fun q -> raw c "pop" [ R q ]) (List.rev saves);
    reset c;
    (match r with Some (x, t) when SS.mem x k.lv -> st c t x | _ -> ());
    term c k
    end
  | L.Switch (v, cases, other) ->
    let targets = List.map snd cases @ Option.to_list other in
    set_live c (List.fold_left (fun s k -> SS.union s (snd (Hashtbl.find c.konts k))) SS.empty targets);
    to_a c v;
    let w = c.w and n = List.length cases in
    (* dec a を使う形は使わない形より悪くならないので、まずそちら。A を壊せないときだけ cp で比べる。 *)
    let tree ~leaf cs () =
      let unit = c.w /. float (List.length targets) in    (* 枝 1 つあたりの重み *)
      let go dec () = tree c v other ~leaf ~dec unit cs 0 255 0 in
      if c.pick then attempt c (go true) (go false) else go false ()
    in
    let sorted = List.sort Stdlib.compare cases in
    (* 出し方の候補。順に比べる（書いた順）、2 分探索（葉の大きさを変えて）、表（1 ページ版と 2 ページ版）。 *)
    let ways =
      ("lin", tree ~leaf:max_int cases)
      :: List.filter_map (fun leaf -> if leaf < n then Some ("bin", tree ~leaf sorted) else None) [ 1; 2; 3; 4 ]
      @ [ "tab", (fun () -> table ~v2:false c other sorted);
          "tab2", (fun () -> table ~v2:true c other sorted) ]
    in
    let ways =
      match !only with
      | Some o -> List.filter (fun (name, _) -> name = o) ways
      | None -> if c.pick then ways else [ List.hd ways ]
    in
    (* 全部出してみて、いちばん安いものを採る。
       複製の中では、元の場所の重みで選ぶ。どの複製も元と同じ出し方になり、表も 1 つで済む。 *)
    Option.iter (fun ws -> c.w <- ws) c.wsel;
    let best =
      List.fold_left
        (fun best (_, f) ->
          match trial c f, best with
          | Some (p, _), Some (q, _) when p >= q -> best
          | Some (p, _), _ -> Some (p, f)
          | None, _ -> best)
        None ways
    in
    c.w <- w;
    (match best with Some (_, f) -> f () | None -> raise (Stuck "分岐を出せない"));
    c.w <- w
  | L.LetK (_, body) when c.dup -> term c body         (* 複製では、中の継続は元のものを使う *)
  | L.LetK (ks, body) ->
    (* 本体が値での分岐で終わるなら、その飛び先の継続は「通る割合」だけ軽く数える *)
    let rec share (t : L.t) =
      match t.n with
      | L.Let (_, _, _, k) | L.Store (_, _, _, k) | L.Out (_, _, k) -> share k
      | L.Switch (_, cases, other) ->
        let all = List.map snd cases @ Option.to_list other in
        fun k -> float (max 1 (List.length (List.filter (( = ) k) all))) /. float (List.length all)
      | _ -> fun _ -> 1.
    in
    let share = share body in
    List.iter
      (fun (k : L.kont) ->
        Hashtbl.replace c.konts k.k (k.ps, k.body.lv);
        Hashtbl.replace c.bodies k.k (k.body, c.w *. share k.k *. if k.loop then 8. else 1.);
        match k.body.n with
        | L.If (o, ty, x, y, _, ({ n = L.Jump _ | L.Ret _; _ } as no)) when k.loop ->
          Hashtbl.replace c.heads k.k (o, ty, x, y, c.f ^ "." ^ k.k ^ ".body", no)
        | _ -> ())
      ks;
    (* 継続を先に出しておき、本体の後ろに並べる。本体の最後が値での分岐なら、
       飛び先までの距離を見て出し方を選べる。継続の入口では何も当てにしないので、順番は入れ替えてよい。 *)
    let back = mark c and next = c.next in
    c.out <- [];
    c.ofs <- [];
    let w = c.w in
    List.iter
      (fun (k : L.kont) ->
        c.w <- w *. share k.k;
        if k.used then begin
          rem c (Printf.sprintf "cont %s(%s)" k.k (show_params c k.ps));
          label c (c.f ^ "." ^ k.k);
          reset c;                                    (* どこから来たか分からない *)
          match Hashtbl.find_opt c.heads k.k, k.body.n with
          | Some (o, ty, x, y, yes_l, no), L.If (_, _, _, _, yes, _) ->
            (* ループの先頭。本体にラベルを付けておき、戻るときはそこへ直接飛べるようにする。 *)
            set_live c k.body.lv;
            let cc = compare c o ty x y in
            let out = fresh c "exit" in
            raw c "jp" [ R (neg_cc cc); I (Sym out) ];
            rem c (Printf.sprintf "loop %s(%s)" k.k (show_params c k.ps));
            label c yes_l;
            reset c;
            c.w <- c.w *. 8.;
            term c yes;
            c.w <- c.w /. 8.;
            label c out;
            reset c;
            term c no
          | _ ->
            if k.loop then c.w <- c.w *. 8.;
            term c k.body;
            if k.loop then c.w <- c.w /. 8.
        end)
      ks;
    c.w <- w;
    let ks, n, data, tabs = c.out, c.n, c.data, c.tabs in
    back ();
    c.n <- n;
    c.data <- data;
    c.tabs <- tabs;
    c.next <- lazy (first 130 (List.rev ks @ Lazy.force next));
    term c body;
    c.next <- next;
    c.out <- ks @ c.out
  | L.Jump (k, args) ->
    let ps, live = Hashtbl.find c.konts k in
    pmove c live (List.map2 (fun (p, t) v -> loc c t (V p), t, loc c t v, Some p) ps args);
    set_live c live;
    (match Hashtbl.find_opt c.heads k with
     | Some (o, ty, x, y, yes_l, no) when not c.norot ->
       (* ループの先頭へ戻る代わりに、ここで条件を調べて本体へ直接飛ぶ。
          直前の演算のフラグが残っていれば、比較の命令も要らない。
          渡した値について覚えていることは、受け取る側の変数名に付け替える。 *)
       let rename v =
         match List.find_opt (fun (_, w) -> Some w = v) (List.combine ps args) with
         | Some ((p, _), _) -> Some (V p)
         | None -> (match v with Some (V _) -> None | v -> v)
       in
       c.ca <- rename c.ca;
       c.chl <- rename c.chl;
       c.hnear <- None;
       c.zf <- (match rename (Option.map (fun z -> V z) c.zf) with Some (V p) -> Some p | _ -> None);
       set_live c live;
       (* 比べる値が、いま渡した定数なら、条件はここで決まる（wait 48 の最初の「0 か」など） *)
       let given = function
         | V p -> (match List.find_opt (fun ((q, _), _) -> q = p) (List.combine ps args) with Some (_, C n) -> Some n | _ -> None)
         | C n -> Some n
         | S _ -> None
       in
       let known =
         match given x, given y with
         | Some a, Some b ->
           let m = if ty = U8 then 0xff else 0xffff in
           let a = a land m and b = b land m in
           Some (match o with "==" -> a = b | "!=" -> a <> b | "<" -> a < b | ">=" -> a >= b | ">" -> a > b | _ -> a <= b)
         | _ -> None
       in
       let exit () = c.zf <- None; c.norot <- true; term c no; c.norot <- false in
       (match known with
        | Some true -> raw c "jp" [ I (Sym yes_l) ]
        | Some false -> exit ()
        | None ->
          let cc = compare c o ty x y in
          raw c "jp" [ R cc; I (Sym yes_l) ];
          exit ())
     | _ ->
       (* 飛び先が短い一本道で、値での分岐で終わるなら、飛ぶ代わりにその場へ複製する。
          jp の時間が浮き、その分だけ大きくなる。得かどうかは値段で決める。
          （バイトコードを読んで分岐するループなら、命令ごとの「先頭へ戻る」がなくなる。） *)
       let rec straight d (t : L.t) =
         match t.n with
         | L.Let (_, _, _, k) | L.Store (_, _, _, k) | L.Out (_, _, k) | L.LetK (_, k) -> straight d k
         | L.Switch (_, cases, other) ->               (* 飛び先がもう分かっていること *)
           List.for_all (Hashtbl.mem c.konts) (List.map snd cases @ Option.to_list other)
         | L.Jump (k, _) when d < 3 ->
           (match Hashtbl.find_opt c.bodies k with Some (b, _) -> not (Hashtbl.mem c.heads k) && straight (d + 1) b | None -> false)
         | _ -> false
       in
       let jump () = raw c "jp" [ klabel c k ] in
       (match Hashtbl.find_opt c.bodies k with
        | Some (body, wk) when c.pick && straight 0 body ->
          let copy () =
            let d = c.dup and ws = c.wsel in
            c.dup <- true;
            c.wsel <- Some wk;
            reset c;                                  (* 複製の中で付け直される変数のことは、忘れておく *)
            term c body;
            c.dup <- d;
            c.wsel <- ws
          in
          (* jp 1 つの値段（時間 × 重み + 3 バイト）より、複製の大きさが小さければ複製する *)
          let j = Cost.price [ Ins ("jp", [ klabel c k ]), c.w ] [] in
          (match trial c copy with Some (_, bytes) when bytes < j -> copy () | _ -> jump ())
        | _ -> jump ()))
  | L.If (o, ty, x, y, yes, no) ->
    set_live c t.lv;
    let cc = compare c o ty x y in
    c.zf <- None;                                     (* 分岐の先ではフラグを当てにしない *)
    (match yes.n, no.n with
     | L.Jump (k, args), _ when pure c k args -> jcc c cc k; term c no
     | L.Ret None, _ when not c.task -> raw c "ret" [ R cc ]; term c no
     | _, L.Jump (k, args) when pure c k args -> jcc c (neg_cc cc) k; term c yes
     | _, L.Ret None when not c.task -> raw c "ret" [ R (neg_cc cc) ]; term c yes
     | _ ->
       let l = fresh c "else" in
       raw c "jp" [ R (neg_cc cc); I (Sym l) ];
       let ca, chl = c.ca, c.chl in
       (* 枝はどちらか一方しか通らないので、半分ずつに数える。else if が続くと、奥ほど軽くなる。
          同じ重みで数えると、枝ごとに要る push/pop が段の数だけ重く見え、ループの変数をメモリへ追いやった
          （damaxian の EnemyUpdate） *)
       let w = c.w in
       c.w <- w /. 2.;
       term c yes;
       label c l;
       c.ca <- ca; c.chl <- chl; c.hnear <- None; c.zf <- None;
       term c no;
       c.w <- w)
  | L.Ret None -> if c.task then (set_live c t.lv; raw c "jp" [ I (Sym (c.f ^ ".done")) ]) else raw c "ret" []
  | L.Yield k ->
    (* 中断: 続きの番地を覚えて、resume した側へ戻る。レジスタの中身は残らないので、
       中断をまたいで生きる変数はメモリに置いてあること。
       プールのタスクは個体ごとのフレームに置く必要があり、型検査がそうしている。 *)
    SS.iter
      (fun x ->
        if Some x = c.self then ()
        else if c.self <> None then
          raise (Ast.Error (0, Printf.sprintf "%s: 中断をまたぐ値がフレームにない（%s）" c.f x))
        else if not (is_mem (c.home x)) then raise (Stuck ("中断をまたぐ変数はメモリに置く: " ^ x)))
      k.lv;
    set_live c SS.empty;
    let l = fresh c "resume" in
    set_pc c (Some l);
    raw c "ret" [];
    rem c "resume";
    label c l;
    reset c;
    enter c;
    term c k
  | L.Ret (Some (v, ty)) ->
    set_live c t.lv;
    if ty = U8 then to_a c v else to_hl c v;
    c.held <- None;
    raw c "ret" []

(* 関数 1 つ。home が置き場所の割り当て。出せなければ Stuck。 *)
let fn ~info ~exts ~home ~pick (f : Ir.fn) (body : L.t) : (line * float) list * line list =
  let c = { f = f.name; task = f.task; self = f.self; home; konts = Hashtbl.create 16; info; exts; out = []; w = 1.;
            busy = []; ca = None; chl = None; hnear = None; zf = None; n = 0;
            heads = Hashtbl.create 8; norot = false; pick; data = []; bodies = Hashtbl.create 16; wsel = None; dup = false; tabs = []; next = lazy []; ofs = []; walking = false; last = None; held = None } in
  (* 引数どうしが同じ場所を取り合っていないこと *)
  let rec distinct = function
    | [] -> ()
    | (p, _) :: rest ->
      List.iter
        (fun (q, _) -> if List.exists (fun x -> List.mem x (places (home q))) (places (home p)) then raise (Stuck "引数が重なる"))
        rest;
      distinct rest
  in
  distinct f.ps;
  rem c (Printf.sprintf "%s %s(%s)%s" (if f.task then "task" else "fn") f.name (show_params c f.ps)
           (match f.ret with Some U8 -> " -> A" | Some U16 -> " -> HL" | None -> ""));
  label c f.name;
  enter c;
  term c body;
  if f.task then begin
    (* 終わったタスク: 「終わった」と書いて戻る。1 個だけのタスクは続きの番地をここに向けるので、
       resume されても何もせずに戻る。プールのタスクは枠が空きになる。 *)
    label c (f.name ^ ".done");
    reset c;
    set_pc c None;
    raw c "ret" []
  end;
  List.rev c.out, c.data
