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
module Const_fold = Const_fold
module Cse = Cse
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

(* 16KB に入らないときは 32KB（0x4000〜0xbfff）にする。BIOS が開くのは 0x4000〜0x7fff（ページ 1）だけなので、
   ヘッダのすぐ後ろに、自分のスロットをページ 2 にも出すコードを置く。
   RSLREG（0x138）で今の基本スロットを読み、ページ 1 のスロット番号を取り出す。拡張されたスロットなら
   EXPTBL（0xfcc1）の上位ビットと SLTTBL（0xfcc5）の拡張スロット番号を足して、ENASLT（0x24）でページ 2 に出す。 *)
let page2 =
  let open Asm in
  let ins op args = Ins (op, args) and r x = R x and n v = I (Num v) in
  [ ins "call" [ n 0x138 ]; ins "rrca" []; ins "rrca" []; ins "and" [ n 3 ]; ins "ld" [ r "c"; r "a" ];
    ins "ld" [ r "b"; n 0 ]; ins "ld" [ r "hl"; n 0xfcc1 ]; ins "add" [ r "hl"; r "bc" ];
    ins "ld" [ r "a"; M (r "hl") ]; ins "and" [ n 0x80 ]; ins "or" [ r "c" ]; ins "ld" [ r "c"; r "a" ];
    ins "inc" [ r "hl" ]; ins "inc" [ r "hl" ]; ins "inc" [ r "hl" ]; ins "inc" [ r "hl" ];
    ins "ld" [ r "a"; M (r "hl") ]; ins "and" [ n 0x0c ]; ins "or" [ r "c" ];
    ins "ld" [ r "h"; n 0x80 ]; ins "call" [ n 0x24 ] ]

let msx32 : Alloc.target =
  { msx with
    head = Asm.[ Db [ Num 0x41; Num 0x42 ]; Dw [ Sym ".msx32" ]; Ds 12; Label ".msx32" ]
           @ page2 @ [ Asm.Ins ("jp", [ I (Sym "start") ]) ] }

(* メガロム（ASCII8）: 8KB のバンクを 0x4000、0x6000、0x8000、0xa000 に出す。バンクの番号は 0x6000、0x6800、0x7000、
   0x7800 に書く。プログラムは 24KB まで、必要なら 32KB まで置ける。-bank のデータバンクはプログラム長の後ろに続く。 *)
let msx8 : Alloc.target =
  let open Asm in
  let ins op args = Ins (op, args) and r x = R x and n v = I (Num v) in
  { msx with
    head = [ Db [ Num 0x41; Num 0x42 ]; Dw [ Sym ".msx8" ]; Ds 12; Label ".msx8" ]
           @ page2
           @ List.concat_map (fun (a, b) -> [ ins "ld" [ r "a"; n b ]; ins "ld" [ M (n a); r "a" ] ])
               [ 0x6800, 1; 0x7000, 2; 0x7800, 3 ]
           @ [ ins "jp" [ I (Sym "start") ] ] }

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
(* 構文木から ROM まで *)
let build ?(target = msx) ?(opt = true) tops =
  let prog = Lut.apply (Check.check tops) in
  let ir = Cps.convert prog in
  let size s =                                 (* 表や配列のバイト数 *)
    let w t = if Ast.wide t then 2 else 1 in
    match List.find_opt (fun (x, _, _) -> x = s) prog.arrays, List.find_opt (fun (x, _, _) -> x = s) prog.datas with
    | Some (_, t, n), _ -> Some (n * w t)
    | _, Some (_, t, vs) -> Some (List.length vs * w t)
    | None, None -> None
  in
  let ir = if opt then Inline.program ir else ir in
  let ir = if opt then List.map (fun f -> Cse.fn (Ptr.fn size (Iv.fn (Blk.fn (Cse.fn (Narrow.fn (Cse.fn f))))))) ir else ir in
  let asm1 target =
    let asm = Alloc.program ~opt target prog ir in
    let bin, syms = try Asm.assemble ~org:target.org asm with Asm.Error m -> raise (Ast.Error (0, m)) in
    asm, bin, syms
  in
  let asm, bin, syms = asm1 target in
  (* MSX で 16KB を超えたら、32KB の ROM として作り直す *)
  let target, (asm, bin, syms) =
    if target == msx && String.length bin > 0x4000 then msx32, asm1 msx32 else target, (asm, bin, syms)
  in
  { prog; ir; asm; bin; syms; org = target.org; target }

let compile ?target ?opt ?read src =
  let tops =
    match read with
    | None -> Parse.parse src
    | Some read -> files := []; load (fun f -> if f = "" then src else read f) ""
  in
  build ?target ?opt tops

(* C のファイル（いくつでも）をコンパイルする。read はパスから中身（なければ None）、incs は #include を探す所。
   #include "x.hl" は hlang のファイルを取り込む（hl_read で探す）。 *)
let compile_c ?target ?opt ~(read : string -> string option) ~(hl_read : string -> string) ~incs paths =
  Cpp.files := [];
  Cparse.reset ();
  let find dirs name =
    List.find_map (fun d -> let p = if d = "" then name else Filename.concat d name in Option.map (fun s -> p, s) (read p))
      (if Filename.is_relative name then dirs else [ "" ])
  in
  let ctops =
    List.map
      (fun p ->
        match read p with
        | Some src ->
          let ts = Cpp.run ~read:find ~incs p src in
          files := !Cpp.files;                             (* エラーの行番号をファイル名に戻せるように *)
          Cparse.parse ts
        | None -> raise (Ast.Error (0, "ファイルがない: " ^ p)))
      paths
  in
  (* hlang のファイルの関数の型（引数と戻り）。中の import もたどる *)
  let rec sigs seen f =
    if List.mem f !seen then []
    else begin
      seen := f :: !seen;
      List.concat_map
        (function
          | Ast.Fn (fn, _) -> [ fn.name, (List.map snd fn.ps, fn.ret) ]
          | Ast.Extern (name, args, ret, _, _, _) -> [ name, (List.map fst args, ret) ]
          | Ast.Import (g, _) -> sigs seen g
          | _ -> [])
        (Parse.parse (hl_read f))
    end
  in
  let tops = Clower.program ~hl_sigs:(fun f -> sigs (ref []) f) ctops in
  (* hlang のファイルを取り込む。行番号は C のファイルの続きから *)
  files := !Cpp.files;
  let tops = List.concat_map (function Ast.Import (f, _) -> load hl_read f | t -> [ t ]) tops in
  build ?target ?opt tops

(* ROM イメージ。余りは 0xff で埋める。MSX は 16KB（入らなければ 32KB）。
   SMS は 32KB で、最後の 16 バイトに "TMR SEGA" の印と、そこまでの全バイトの和を置く（海外版の本体が起動時に調べる）。 *)
let rom r =
  let fill size =
    if String.length r.bin > size then raise (Ast.Error (0, "ROM に入りきらない"));
    r.bin ^ String.make (size - String.length r.bin) '\xff'
  in
  if r.target == msx32 then fill 0x8000
  else if r.target == msx8 then fill (if String.length r.bin > 0x6000 then 0x8000 else 0x6000)
  else if r.target != sms then fill 0x4000
  else begin
    let body = fill 0x7ff0 in
    let sum = String.fold_left (fun n c -> (n + Char.code c) land 0xffff) 0 body in
    body ^ "TMR SEGA\000\000" ^ String.init 2 (fun i -> Char.chr ((sum lsr (8 * i)) land 255)) ^ "\000\000\000\x4c"
  end
