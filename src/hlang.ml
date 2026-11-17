(* コンパイラの入口。ソースの文字列から ROM までを順につなぐ。 *)

module Asm = Asm
module Dis = Dis
module Zma = Zma

module Cost = Cost
module Ast = Ast
module Parse = Parse
module Check = Check
module Interp = Interp
module Lut = Lut
module Ir = Ir
module Cps = Cps
module Opt = Opt
module Ptr = Ptr
module Report = Report
module Live = Live
module Gen = Gen
module Alloc = Alloc
module Run = Run

(* MSX の ROM カートリッジ: 0x4000 から 16KB。先頭 16 バイトがヘッダ。 *)
let msx : Alloc.target = {
  org = 0x4000;
  ram = 0xe000;
  head = Asm.[ Db [ Num 0x41; Num 0x42 ]; Dw [ Sym "start" ]; Ds 12 ];
}

(* セガ・マスターシステム: 0 番地から 32KB。RAM は 0xc000 から。
   0 番地が起動、0x38 が画面の割り込み、0x66 がポーズボタン（NMI）と決まっているので、先頭にその 3 つを置く。
   割り込みはレジスタを全部しまい、VDP の状態を読んで（読まないと次が来ない）、RAM のフック sms_hook を呼ぶ。
   フックは起動時に ret にしておき、lib/sms.hl の on_vsync が jp に書き換える。 *)
let sms_hook = 0xdff8
let sms : Alloc.target =
  let open Asm in
  let ins op args = Ins (op, args) and r x = R x and n v = I (Num v) in
  let regs = [ "af"; "bc"; "de"; "hl"; "ix"; "iy" ] in
  let boot = [
    ins "di" []; ins "im" [ n 1 ]; ins "ld" [ r "sp"; n 0xdff0 ];
    ins "ld" [ r "hl"; n 0xfffc ];                               (* ROM の切り替え（マッパー）を初めの状態に *)
    ins "ld" [ M (r "hl"); n 0 ]; ins "inc" [ r "hl" ]; ins "ld" [ M (r "hl"); n 0 ]; ins "inc" [ r "hl" ];
    ins "ld" [ M (r "hl"); n 1 ]; ins "inc" [ r "hl" ]; ins "ld" [ M (r "hl"); n 2 ];
    ins "ld" [ r "a"; n 0xc9 ]; ins "ld" [ M (n sms_hook); r "a" ];
    ins "jp" [ I (Sym "start") ] ]
  and irq =
    List.map (fun x -> ins "push" [ r x ]) regs
    @ [ ins "in" [ r "a"; M (n 0xbf) ]; ins "call" [ n sms_hook ] ]
    @ List.map (fun x -> ins "pop" [ r x ]) (List.rev regs)
    @ [ ins "ei" []; ins "ret" [] ]
  in
  let size l = String.length (fst (assemble ~org:0 (Label "start" :: l))) in
  { org = 0; ram = 0xc000;
    head = boot @ [ Ds (0x38 - size boot) ] @ irq @ [ Ds (0x66 - 0x38 - size irq); ins "retn" [] ] }

type result = {
  prog : Check.prog;
  ir : Ir.fn list;
  asm : Asm.line list;
  bin : string;                                (* 組み立てたままのバイト列 *)
  syms : (string * int) list;
  org : int;
  target : Alloc.target;
}

(* エミュレータで走らせる。extern 関数は ext が肩代わりする（参照インタプリタと同じ形）。 *)
let run ?limit ?io r ~ext =
  Run.exec ?limit ?io ~org:r.org ~start:(List.assoc "start" r.syms) r.bin (Run.traps r.prog.exts ext)

(* import をたどって、1 つの構文木にする。read はファイル名から中身を返す。同じファイルは 1 回だけ読む。
   行番号はファイルごとに 100000 ずつずらしてあり、where でファイル名と行に戻せる。 *)
let files : string list ref = ref []

let rec load read name =
  if List.mem name !files then []
  else begin
    files := !files @ [ name ];
    let base = (List.length !files - 1) * 100000 in
    List.concat_map (function Ast.Import (f, _) -> load read f | t -> [ t ]) (Parse.parse ~base (read name))
  end

let where line = (try List.nth !files (line / 100000) with _ -> ""), line mod 100000

(* read を渡せば import が使える。src は最初のファイルの中身。 *)
let compile ?(target = msx) ?(opt = true) ?read src =
  let tops =
    match read with
    | None -> Parse.parse src
    | Some read -> files := []; load (fun f -> if f = "" then src else read f) ""
  in
  let prog = Lut.apply (Check.check tops) in
  let ir = Cps.convert prog in
  let size s =                                 (* 表や配列のバイト数 *)
    let w t = if Ast.wide t then 2 else 1 in
    match List.find_opt (fun (x, _, _) -> x = s) prog.arrays, List.find_opt (fun (x, _, _) -> x = s) prog.datas with
    | Some (_, t, n), _ -> Some (n * w t)
    | _, Some (_, t, vs) -> Some (List.length vs * w t)
    | None, None -> None
  in
  let ir = if opt then List.map (fun f -> Opt.fn (Ptr.fn size (Blk.fn (Opt.fn f)))) ir else ir in
  let asm = Alloc.program ~opt target prog ir in
  let bin, syms =
    try Asm.assemble ~org:target.org asm with Asm.Error m -> raise (Ast.Error (0, m))
  in
  { prog; ir; asm; bin; syms; org = target.org; target }

(* ROM イメージ。余りは 0xff で埋める。MSX は 16KB。
   SMS は 32KB で、最後の 16 バイトに "TMR SEGA" の印と、そこまでの全バイトの和を置く（海外版の本体が起動時に調べる）。 *)
let rom r =
  let fill size =
    if String.length r.bin > size then raise (Ast.Error (0, "ROM に入りきらない"));
    r.bin ^ String.make (size - String.length r.bin) '\xff'
  in
  if r.target != sms then fill 0x4000
  else begin
    let body = fill 0x7ff0 in
    let sum = String.fold_left (fun n c -> (n + Char.code c) land 0xffff) 0 body in
    body ^ "TMR SEGA\000\000" ^ String.init 2 (fun i -> Char.chr ((sum lsr (8 * i)) land 255)) ^ "\000\000\000\x4c"
  end
