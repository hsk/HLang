(* コンパイラの入口。ソースの文字列から ROM までを順につなぐ。 *)

module Asm = Asm
module Dis = Dis
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

type result = {
  prog : Check.prog;
  ir : Ir.fn list;
  asm : Asm.line list;
  bin : string;                                (* 組み立てたままのバイト列 *)
  syms : (string * int) list;
  org : int;
}

(* エミュレータで走らせる。extern 関数は ext が肩代わりする（参照インタプリタと同じ形）。 *)
let run ?limit r ~ext =
  Run.exec ?limit ~org:r.org ~start:(List.assoc "start" r.syms) r.bin (Run.traps r.prog.exts ext)

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
  let ir = if opt then List.map (fun f -> Opt.fn (Ptr.fn size (Opt.fn f))) ir else ir in
  let asm = Alloc.program ~opt target prog ir in
  let bin, syms =
    try Asm.assemble ~org:target.org asm with Asm.Error m -> raise (Ast.Error (0, m))
  in
  { prog; ir; asm; bin; syms; org = target.org }

(* 16KB の ROM イメージ。余りは 0xff で埋める。 *)
let rom r =
  let size = 0x4000 in
  if String.length r.bin > size then raise (Ast.Error (0, "ROM に入りきらない"));
  r.bin ^ String.make (size - String.length r.bin) '\xff'
