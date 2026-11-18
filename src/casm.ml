(* SDCC のインラインアセンブラ（sdasz80 の書き方）を、組み立ての行（Asm.line）にする。
   字句はプリプロセッサがマクロを展開した後のもの（Cpp の Asm の中身。行の区切りは Nl）。
   1 文ずつ ZMA の書き方のテキストに直して、Zma.parse_line で読む。
   - #0x10 の # は即値の印なので取る。数は値にしてから渡す（sdasz80 の 010 は 10 進、ZMA は 8 進なので）
   - _name は C の名前。sym で hlang のラベルに直す
   - 0$: のような数のラベルは、その関数の中だけのラベルにする（普通のラベルの間だけで有効）
   - or a, x のように A を書いた形は、or x にする
   - マクロは \ で行をつなぐので、1 行に命令がいくつも並ぶ（ld a, x ld y, a）。命令の名前で区切る *)

open Cpp

(* 文字列 s の中で、pat の後ろの部分 *)
module Str_find = struct
  let after s pat =
    let n = String.length s and m = String.length pat in
    let rec go i = if i + m > n then None else if String.sub s i m = pat then Some (String.sub s (i + m) (n - i - m)) else go (i + 1) in
    go 0
end

let mnemonics = [
  "ld"; "push"; "pop"; "ex"; "exx"; "ldi"; "ldir"; "ldd"; "lddr"; "cpi"; "cpir"; "cpd"; "cpdr";
  "add"; "adc"; "sub"; "sbc"; "and"; "or"; "xor"; "cp"; "inc"; "dec"; "daa"; "cpl"; "neg"; "ccf"; "scf";
  "nop"; "halt"; "di"; "ei"; "im"; "rlca"; "rla"; "rrca"; "rra"; "rlc"; "rl"; "rrc"; "rr"; "sla"; "sra"; "sll"; "srl";
  "rld"; "rrd"; "bit"; "set"; "res"; "jp"; "jr"; "djnz"; "call"; "ret"; "reti"; "retn"; "rst";
  "in"; "ini"; "inir"; "ind"; "indr"; "out"; "outi"; "otir"; "outd"; "otdr";
]

let is_mn = function Id m -> List.mem (String.lowercase_ascii m) mnemonics | _ -> false

(* 数の綴り（0x10 0b0101 12 0h など）。$ で終わるものは数のラベル *)
let number ln s =
  let l = String.lowercase_ascii s in
  let n = String.length l in
  let v =
    if n > 2 && (String.sub l 0 2 = "0x" || String.sub l 0 2 = "0h") then int_of_string_opt ("0x" ^ String.sub l 2 (n - 2))
    else if n > 2 && String.sub l 0 2 = "0b" then int_of_string_opt l
    else if n > 2 && (String.sub l 0 2 = "0o" || String.sub l 0 2 = "0q") then int_of_string_opt ("0o" ^ String.sub l 2 (n - 2))
    else if n > 1 && l.[n - 1] = 'h' then int_of_string_opt ("0x" ^ String.sub l 0 (n - 1))
    else int_of_string_opt l
  in
  match v with Some v -> v | None -> Ast.err ln "アセンブラの数が読めない: %s" s

let is_local s = String.length s > 1 && s.[String.length s - 1] = '$'

(* 文（ラベル 1 つ、または命令・指令 1 つ）ごとに分ける *)
let statements (ts : tok list) : tok list list =
  let out = ref [] and cur = ref [] in
  let flush () = if !cur <> [] then out := List.rev !cur :: !out; cur := [] in
  let rec go = function
    | [] -> flush ()
    | { k = Nl; _ } :: r -> flush (); go r
    | { k = P ";"; _ } :: r ->                       (* ; から行末は注釈 *)
      let rec skip = function { k = Nl; _ } :: r -> r | _ :: r -> skip r | [] -> [] in
      flush (); go (skip r)
    | ({ k = Id _ | Num _; _ } as l) :: { k = P ":"; _ } :: r ->
      (* ラベル（name: か name::）。オペランドに : は出てこない *)
      flush ();
      out := [ l ] :: !out;
      go (match r with { k = P ":"; _ } :: r -> r | r -> r)
    | ({ k; _ } as t) :: r when is_mn k || k = P "." ->
      (* 命令の名前（か . で始まる指令）は、前の文の続き（カンマの後など）でなければ、新しい文の始まり *)
      (match !cur with
       | { k = P ("," | "(" | "+" | "-" | "." | "#"); _ } :: _ -> cur := t :: !cur
       | [] -> cur := [ t ]
       | _ -> flush (); cur := [ t ]);
      go r
    | t :: r -> cur := t :: !cur; go r
  in
  go ts;
  List.rev !out

(* asm の字句から、組み立ての行を作る。
   name: この asm の持ち主（数のラベルの頭に付ける）。sym: C の名前 -> hlang のラベル *)
let lines ~name ~(sym : int -> string -> string) (ts : tok list) : Asm.line list =
  let scope = ref 0 in
  let local ln s = Printf.sprintf "%s.%d_%d" name !scope (number ln (String.sub s 0 (String.length s - 1))) in
  let spell (t : tok) =
    match t.k with
    | P "#" -> ""
    | Num s when is_local s -> local t.ln s
    | Num s -> string_of_int (number t.ln s)
    | Id x when String.length x > 1 && x.[0] = '_' -> sym t.ln (String.sub x 1 (String.length x - 1))
    | Id x -> x
    | Cpp.Chr c -> string_of_int c
    | Cpp.Str s -> "\"" ^ s ^ "\""
    | P p -> p
    | k -> spell k
  in
  let join ts = String.concat "" (List.mapi (fun i (t : tok) -> (if i > 0 && t.sp then " " else "") ^ spell t) ts) in
  (* オペランドの d(ix) を (ix+d) にする。オペランドは括弧の外のカンマで分ける *)
  let operand ts =
    match List.rev ts with
    | { k = P ")"; _ } :: { k = Id (("ix" | "iy") as r); _ } :: { k = P "("; _ } :: (_ :: _ as d) -> "(" ^ r ^ "+" ^ join (List.rev d) ^ ")"
    | _ ->
      (* #(式) は即値。括弧で始まると ZMA はメモリの参照と読むので、0+ を前に付ける *)
      match ts with
      | { k = P "#"; _ } :: ({ k = P "("; _ } :: _ as r) -> "0+" ^ join r
      | _ -> join ts
  in
  let rec split depth cur acc = function
    | [] -> List.rev (List.rev cur :: acc)
    | ({ k = P ","; _ }) :: r when depth = 0 -> split depth [] (List.rev cur :: acc) r
    | ({ k = P "("; _ } as t) :: r -> split (depth + 1) (t :: cur) acc r
    | ({ k = P ")"; _ } as t) :: r -> split (depth - 1) (t :: cur) acc r
    | t :: r -> split depth (t :: cur) acc r
  in
  let text = function
    | [] -> ""
    | op :: args -> spell op ^ (if args = [] then "" else " " ^ String.concat ", " (List.map operand (split 0 [] [] args)))
  in
  (* Zma の「N 行目: 」は付けない（呼ぶ側がファイルと行を付ける） *)
  let msg m = match Str_find.after m "行目: " with Some r -> r | None -> m in
  (* or a, x などは、ZMA（と hlang のアセンブラ）では or x *)
  let fix = function
    | Asm.Ins (op, [ Asm.R "a"; x ]) when List.mem op [ "sub"; "and"; "or"; "xor"; "cp" ] -> Asm.Ins (op, [ x ])
    | l -> l
  in
  (* sdasz80 の文字定数 'x（閉じない）を数にする。af' の ' はそのまま *)
  let rec chars = function
    | ({ k = Id "af"; _ } as a) :: ({ k = P "'"; _ } as q) :: r -> a :: q :: chars r
    | ({ k = P "'"; _ } as q) :: ({ k = (Id s | Num s); _ }) :: r when String.length s = 1 -> { q with k = Cpp.Chr (Char.code s.[0]) } :: chars r
    | t :: r -> t :: chars r
    | [] -> []
  in
  List.concat_map
    (fun (st : tok list) ->
      let st = chars st in
      let ln = (List.hd st).ln in
      match st with
      | [ { k = Num s; _ } ] when is_local s -> [ Asm.Label (local ln s) ]
      | [ { k = Id x; _ } as t ] when not (is_mn t.k) ->
        incr scope;                                   (* 普通のラベルで、数のラベルの有効な範囲が切り替わる *)
        [ Asm.Label (if String.length x > 1 && x.[0] = '_' then sym ln (String.sub x 1 (String.length x - 1)) else x) ]
      | { k = P "."; _ } :: ({ k = Id d; _ } as t) :: rest ->
        let d = match String.lowercase_ascii d with "db" | "byte" | "ascii" | "str" -> "db" | "dw" | "word" -> "dw" | "ds" | "blkb" -> "ds" | d -> Ast.err ln "知らない指令: .%s" d in
        ignore t;
        let s = d ^ " " ^ String.concat ", " (List.map operand (split 0 [] [] rest)) in
        (try List.map fix (Zma.parse_line ln s) with Asm.Error m -> Ast.err ln "%s（%s）" (msg m) s)
      | _ ->
        let s = text st in
        (try List.map fix (Zma.parse_line ln s) with Asm.Error m -> Ast.err ln "%s（%s）" (msg m) s))
    (statements ts)
