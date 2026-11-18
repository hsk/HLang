(* C の字句解析とプリプロセッサ。ファイルを読んで、マクロを展開した字句の配列にする。
   行番号は hlang と同じく、ファイルごとに 100000 ずつずらす（Hlang.where で戻せる）。 *)

type kind =
  | Id of string
  | Num of string                (* 数の綴り。値は構文解析で読む *)
  | Chr of int                   (* 'a' *)
  | Str of string                (* "..."（\n などは読んだ後） *)
  | Asm of tok list              (* __asm ... __endasm の中身（マクロを展開した字句。行の区切りは Nl） *)
  | Nl                           (* __asm の中の改行 *)
  | P of string                  (* 記号 *)
  | Eof

(* sp: 前に空白があったか（関数形式のマクロの定義を見分けるのと、# で文字列にするのに使う）。
   hide: この字句を作ったマクロ（同じマクロを中で展開し直さない） *)
and tok = { k : kind; ln : int; sp : bool; hide : string list }

let puncts = [
  "<<="; ">>="; "..."; "->"; "++"; "--"; "<<"; ">>"; "<="; ">="; "=="; "!="; "&&"; "||";
  "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "##";
]

let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let spell = function
  | Id s | Num s | P s -> s
  | Chr c -> Printf.sprintf "'%c'" (Char.chr c)
  | Str s -> Printf.sprintf "%S" s
  | Asm _ -> "__asm ... __endasm"
  | Nl -> "\n"
  | Eof -> ""

(* ---- 字句に分ける ---- *)

(* 行頭の # で始まる行は指令。T は普通の字句、D は指令 1 行分の字句（先頭が指令の名前） *)
type item = T of tok | D of int * tok list

let scan base s =
  let n = String.length s in
  let i = ref 0 and line = ref (base + 1) and bol = ref true and sp = ref true in
  let asm = ref false in                       (* __asm の中。改行を Nl に、; から行末を注釈にする *)
  let items = ref [] and dir = ref None in
  let at k c = !i + k < n && s.[!i + k] = c in
  let push k =
    let t = { k; ln = !line; sp = !sp; hide = [] } in
    (match !dir with Some (l, ts) -> dir := Some (l, t :: ts) | None -> items := T t :: !items);
    bol := false;
    sp := false
  in
  let close () = match !dir with Some (l, ts) -> items := D (l, List.rev ts) :: !items; dir := None | None -> () in
  let esc () =
    let c = s.[!i] in
    incr i;
    if c <> '\\' then Char.code c
    else begin
      let e = s.[!i] in
      incr i;
      match e with
      | 'n' -> 10 | 't' -> 9 | 'r' -> 13 | 'a' -> 7 | 'b' -> 8 | 'f' -> 12 | 'v' -> 11
      | 'x' ->
        let j = !i in
        while !i < n && (match s.[!i] with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false) do incr i done;
        int_of_string ("0x" ^ String.sub s j (!i - j)) land 0xff
      | '0' .. '7' ->
        let j = !i - 1 in
        while !i < n && !i - j < 3 && s.[!i] >= '0' && s.[!i] <= '7' do incr i done;
        int_of_string ("0o" ^ String.sub s j (!i - j)) land 0xff
      | c -> Char.code c
    end
  in
  while !i < n do
    let c = s.[!i] in
    if c = '\n' then begin
      if !asm && !dir = None then push Nl;
      incr i; incr line; close (); bol := true; sp := true
    end
    else if c = ';' && !asm && !dir = None then (while !i < n && s.[!i] <> '\n' do incr i done)
    else if c = '\'' && !asm && not (!i + 2 < n && s.[!i + 2] = '\'') then (incr i; push (P "'"))
    else if c = '\\' && at 1 '\n' then (i := !i + 2; incr line)
    else if c = '\\' && at 1 '\r' && at 2 '\n' then (i := !i + 3; incr line)
    else if c = ' ' || c = '\t' || c = '\r' || c = '\012' then (incr i; sp := true)
    else if c = '/' && at 1 '/' then (while !i < n && s.[!i] <> '\n' do incr i done)
    else if c = '/' && at 1 '*' then begin
      i := !i + 2;
      while !i < n && not (s.[!i] = '*' && at 1 '/') do (if s.[!i] = '\n' then incr line); incr i done;
      i := !i + 2;
      sp := true
    end
    else if c = '#' && !bol && !dir = None then (incr i; dir := Some (!line, []); bol := false; sp := true)
    else if is_digit c || (c = '.' && !i + 1 < n && is_digit s.[!i + 1]) then begin
      (* 数の綴り。SDCC と同じく $ も含める（アセンブラのラベル 0$ を 1 つの字句にする） *)
      let j = !i in
      while !i < n && (is_id s.[!i] || s.[!i] = '.' || s.[!i] = '$') do incr i done;
      push (Num (String.sub s j (!i - j)))
    end
    else if is_id c || c = '$' then begin
      (* SDCC は名前に $ を使える *)
      let j = !i in
      while !i < n && (is_id s.[!i] || s.[!i] = '$') do incr i done;
      let w = String.sub s j (!i - j) in
      if !dir = None && (w = "__asm" || w = "_asm") then asm := true;
      if !dir = None && (w = "__endasm" || w = "_endasm") then asm := false;
      push (Id w)
    end
    else if c = '\'' then begin
      incr i;
      let v = esc () in
      if not (at 0 '\'') then Ast.err !line "文字定数が閉じていない";
      incr i;
      push (Chr v)
    end
    else if c = '"' then begin
      incr i;
      let b = Buffer.create 16 in
      while !i < n && s.[!i] <> '"' do
        if s.[!i] = '\n' then Ast.err !line "文字列が閉じていない";
        Buffer.add_char b (Char.chr (esc ()))
      done;
      incr i;
      push (Str (Buffer.contents b))
    end
    else begin
      let fits p = !i + String.length p <= n && String.sub s !i (String.length p) = p in
      let p = match List.find_opt fits puncts with Some p -> p | None -> String.make 1 c in
      i := !i + String.length p;
      push (P p)
    end
  done;
  close ();
  List.rev !items

(* ---- マクロ ---- *)

type macro = Obj of tok list | Fun of string list * bool * tok list   (* 引数, 可変長か, 本体 *)

let macros : (string, macro) Hashtbl.t = Hashtbl.create 64

(* 関数形式のマクロの引数を集める。ts は "(" の次から。戻り値は (引数の並び, 残り) *)
let args ln ts =
  let rec go depth cur acc = function
    | [] -> Ast.err ln "マクロの引数が閉じていない"
    | ({ k = P ")"; _ } as t) :: rest ->
      if depth = 0 then List.rev (List.rev cur :: acc), rest else go (depth - 1) (t :: cur) acc rest
    | ({ k = P "("; _ } as t) :: rest -> go (depth + 1) (t :: cur) acc rest
    | { k = P ","; _ } :: rest when depth = 0 -> go depth [] (List.rev cur :: acc) rest
    | t :: rest -> go depth (t :: cur) acc rest
  in
  match go 0 [] [] ts with
  | [ [] ], rest -> [], rest                                   (* f() *)
  | a, rest -> a, rest

(* 2 つの字句をつなげて 1 つにする（##） *)
let paste ln a b =
  match scan 0 (spell a.k ^ spell b.k) with
  | [ T t ] -> { a with k = t.k }
  | _ -> Ast.err ln "## でつなげた結果が 1 つの字句にならない: %s%s" (spell a.k) (spell b.k)

let rec expand = function
  | [] -> []
  | ({ k = Id name; _ } as t) :: rest when Hashtbl.mem macros name && not (List.mem name t.hide) ->
    let mark body = List.map (fun b -> { b with ln = t.ln; hide = name :: t.hide @ b.hide }) body in
    (match Hashtbl.find macros name, rest with
     | Obj body, _ -> expand (mark body @ rest)
     | Fun (ps, va, body), { k = P "("; _ } :: rest ->
       let a, rest = args t.ln rest in
       let a = if a = [] && ps <> [] then [ [] ] else a in
       let np = List.length ps in
       let a =
         if va then begin
           (* 余った引数は __VA_ARGS__ にまとめる（, も戻す） *)
           let fixed = List.filteri (fun i _ -> i < np - 1) a and more = List.filteri (fun i _ -> i >= np - 1) a in
           let comma = { t with k = P ","; sp = false } in
           fixed @ [ List.concat (List.mapi (fun i x -> if i = 0 then x else comma :: x) more) ]
         end else a
       in
       if List.length a <> np then Ast.err t.ln "マクロ %s の引数は %d 個" name np;
       let arg p = List.assoc_opt p (List.combine ps a) in
       (* 本体の引数を置き換える。# は文字列に、## の両側は展開せずにつなぐ *)
       let rec sub = function
         | [] -> []
         | { k = P "#"; _ } :: ({ k = Id p; _ } as x) :: r when arg p <> None ->
           let s = String.concat "" (List.mapi (fun i a -> (if i > 0 && a.sp then " " else "") ^ spell a.k) (Option.get (arg p))) in
           { x with k = Str s } :: sub r
         | x :: { k = P "##"; _ } :: y :: r ->
           let raw x = match x.k with Id p when arg p <> None -> Option.get (arg p) | _ -> [ x ] in
           let l = raw x and rr = raw y in
           let joined =
             match List.rev l, rr with
             | [], _ -> rr
             | _, [] -> l
             | lx :: lrev, ry :: rrest -> List.rev lrev @ (paste t.ln lx ry :: rrest)
           in
           (match List.rev joined with
            | last :: front when r <> [] && (match List.hd r with { k = P "##"; _ } -> true | _ -> false) ->
              sub (List.rev front @ (last :: r))
            | _ -> joined @ sub r)
         | ({ k = Id p; _ } as x) :: r when arg p <> None ->
           (match expand (Option.get (arg p)) with
            | [] -> sub r
            | e :: es -> ({ e with sp = x.sp } :: es) @ sub r)
         | x :: r -> x :: sub r
       in
       expand (mark (sub body) @ rest)
     | Fun _, _ -> t :: expand rest)
  | t :: rest -> t :: expand rest

(* マクロから出てきた __asm ... __endasm（#define DI() __asm di __endasm など）を、1 つの Asm にまとめる *)
let rec join_asm = function
  | ({ k = Id ("__asm" | "_asm"); _ } as t) :: rest ->
    let rest = match rest with { k = P ";"; _ } :: r -> r | r -> r in      (* __asm; の ; *)
    let rec upto acc = function
      | { k = Id ("__endasm" | "_endasm"); _ } :: r -> List.rev acc, r
      | x :: r -> upto (x :: acc) r
      | [] -> Ast.err t.ln "__asm が __endasm で閉じていない"
    in
    let body, rest = upto [] rest in
    { t with k = Asm body } :: join_asm rest
  | t :: rest -> t :: join_asm rest
  | [] -> []

(* ---- #if の式 ---- *)

let eval_if ln ts =
  (* defined X と defined(X) を先に 1 / 0 にする *)
  let rec defs = function
    | ({ k = Id "defined"; _ } as t) :: { k = P "("; _ } :: { k = Id x; _ } :: { k = P ")"; _ } :: r
    | ({ k = Id "defined"; _ } as t) :: { k = Id x; _ } :: r ->
      { t with k = Num (if Hashtbl.mem macros x then "1" else "0") } :: defs r
    | t :: r -> t :: defs r
    | [] -> []
  in
  let ts = ref (expand (defs ts)) in
  let peek () = match !ts with t :: _ -> t.k | [] -> Eof in
  let adv () = ts := List.tl !ts in
  let num s =
    let s = String.lowercase_ascii s in
    let s = String.concat "" (String.split_on_char 'u' (String.concat "" (String.split_on_char 'l' s))) in
    let s = if String.length s > 1 && s.[0] = '0' && is_digit s.[1] then "0o" ^ String.sub s 1 (String.length s - 1) else s in
    match int_of_string_opt s with Some v -> v | None -> Ast.err ln "#if の数が読めない: %s" s
  in
  let rec prim () =
    match peek () with
    | Num s -> adv (); num s
    | Chr c -> adv (); c
    | Id _ -> adv (); 0                              (* 定義されていない名前は 0 *)
    | P "(" -> adv (); let v = cond () in (if peek () = P ")" then adv () else Ast.err ln "#if の ) がない"); v
    | P "!" -> adv (); if prim () = 0 then 1 else 0
    | P "-" -> adv (); - prim ()
    | P "+" -> adv (); prim ()
    | P "~" -> adv (); lnot (prim ())
    | k -> Ast.err ln "#if の式が読めない: %s" (spell k)
  and bin lv =
    let levels = [| [ "||" ]; [ "&&" ]; [ "|" ]; [ "^" ]; [ "&" ]; [ "=="; "!=" ]; [ "<"; ">"; "<="; ">=" ];
                    [ "<<"; ">>" ]; [ "+"; "-" ]; [ "*"; "/"; "%" ] |] in
    if lv = Array.length levels then prim ()
    else begin
      let l = ref (bin (lv + 1)) in
      let rec loop () =
        match peek () with
        | P o when List.mem o levels.(lv) ->
          adv ();
          let r = bin (lv + 1) in
          let b x = if x then 1 else 0 in
          l := (match o with
              | "||" -> b (!l <> 0 || r <> 0) | "&&" -> b (!l <> 0 && r <> 0)
              | "|" -> !l lor r | "^" -> !l lxor r | "&" -> !l land r
              | "==" -> b (!l = r) | "!=" -> b (!l <> r) | "<" -> b (!l < r) | ">" -> b (!l > r)
              | "<=" -> b (!l <= r) | ">=" -> b (!l >= r) | "<<" -> !l lsl r | ">>" -> !l asr r
              | "+" -> !l + r | "-" -> !l - r | "*" -> !l * r
              | "/" -> if r = 0 then Ast.err ln "#if で 0 で割った" else !l / r
              | _ -> if r = 0 then Ast.err ln "#if で 0 で割った" else !l mod r);
          loop ()
        | _ -> ()
      in
      loop ();
      !l
    end
  and cond () =
    let c = bin 0 in
    if peek () = P "?" then begin
      adv ();
      let a = cond () in
      if peek () <> P ":" then Ast.err ln "#if の ?: に : がない";
      adv ();
      let b = cond () in
      if c <> 0 then a else b
    end else c
  in
  cond () <> 0

(* ---- ファイルを読む ---- *)

let files : string list ref = ref []

let where line = (try List.nth !files (line / 100000) with _ -> ""), line mod 100000

(* read: 探すディレクトリとファイル名から、見つかればそのパスと中身 *)
(* マクロは .c ファイルごとに消す。files は Cparse と同じく、コンパイルの初めに空にしておく *)
let run ~(read : string list -> string -> (string * string) option) ~incs path src =
  Hashtbl.reset macros;
  List.iter (fun (k, v) -> Hashtbl.replace macros k (Obj [ { k = Num v; ln = 0; sp = true; hide = [] } ]))
    [ "__SDCC", "1"; "__SDCC_z80", "1"; "__HLCC__", "1" ];
  let out = ref [] in
  let rec file path src =
    files := !files @ [ path ];
    let base = (List.length !files - 1) * 100000 in
    let dir = Filename.dirname path in
    (* 条件の入れ子: (いま有効か, この #if のどれかの枝を通ったか, 外側が有効か) *)
    let stack = ref [] in
    let live () = match !stack with (a, _, _) :: _ -> a | [] -> true in
    let buf = ref [] in
    let flush () = out := List.rev_append (join_asm (expand (List.rev !buf))) !out; buf := [] in
    List.iter
      (function
        | T t -> if live () then buf := t :: !buf
        | D (ln, ts) ->
          flush ();
          let name, rest = match ts with { k = Id n; _ } :: r -> n, r | { k = Num _; _ } :: _ -> "line", [] | [] -> "", [] | t :: _ -> spell t.k, [] in
          (match name with
           | "ifdef" | "ifndef" | "if" ->
             let v =
               live ()
               && (match name, rest with
                   | "ifdef", { k = Id x; _ } :: _ -> Hashtbl.mem macros x
                   | "ifndef", { k = Id x; _ } :: _ -> not (Hashtbl.mem macros x)
                   | "if", _ -> eval_if ln rest
                   | _ -> Ast.err ln "#%s の名前がない" name)
             in
             stack := (v, v, live ()) :: !stack
           | "elif" ->
             (match !stack with
              | (_, taken, outer) :: r ->
                let v = outer && (not taken) && eval_if ln rest in
                stack := (v, taken || v, outer) :: r
              | [] -> Ast.err ln "#elif に対応する #if がない")
           | "else" ->
             (match !stack with
              | (_, taken, outer) :: r -> stack := (outer && not taken, true, outer) :: r
              | [] -> Ast.err ln "#else に対応する #if がない")
           | "endif" -> (match !stack with _ :: r -> stack := r | [] -> Ast.err ln "#endif に対応する #if がない")
           | _ when not (live ()) -> ()
           | "define" ->
             (match rest with
              | { k = Id m; _ } :: { k = P "("; sp = false; _ } :: r ->
                let rec params acc = function
                  | { k = P ")"; _ } :: body -> List.rev acc, false, body
                  | { k = P "..."; _ } :: { k = P ")"; _ } :: body -> List.rev ("__VA_ARGS__" :: acc), true, body
                  | { k = Id p; _ } :: { k = P ","; _ } :: r -> params (p :: acc) r
                  | { k = Id p; _ } :: ({ k = P ")"; _ } :: _ as r) -> params (p :: acc) r
                  | _ -> Ast.err ln "#define %s の引数が読めない" m
                in
                let ps, va, body = params [] r in
                Hashtbl.replace macros m (Fun (ps, va, body))
              | { k = Id m; _ } :: body -> Hashtbl.replace macros m (Obj body)
              | _ -> Ast.err ln "#define の名前がない")
           | "undef" -> (match rest with { k = Id m; _ } :: _ -> Hashtbl.remove macros m | _ -> ())
           | "include" ->
             let name, sys =
               match expand rest with
               | { k = Str f; _ } :: _ -> f, false
               | { k = P "<"; _ } :: r ->
                 let rec upto acc = function
                   | { k = P ">"; _ } :: _ -> String.concat "" (List.rev acc)
                   | t :: r -> upto (spell t.k :: acc) r
                   | [] -> Ast.err ln "#include の > がない"
                 in
                 upto [] r, true
               | _ -> Ast.err ln "#include のファイル名がない"
             in
             if Filename.check_suffix name ".hl" then
               (* hlang のファイル: hlang のライブラリとして取り込む印を出す *)
               out := List.rev_append [ { k = Id "__hl_import"; ln; sp = true; hide = [] }; { k = Str name; ln; sp = true; hide = [] };
                                        { k = P ";"; ln; sp = false; hide = [] } ] !out
             else if sys && List.mem name [ "stdint.h"; "stdbool.h"; "stddef.h" ] then ()   (* 型は組み込み *)
             else begin
               match read ((if sys then [] else [ dir ]) @ incs) name with
               | Some (p, s) -> file p s
               | None -> Ast.err ln "#include するファイルがない: %s" name
             end
           | "pragma" | "line" | "" | "warning" -> ()
           | "error" -> Ast.err ln "#error %s" (String.concat " " (List.map (fun t -> spell t.k) rest))
           | d -> Ast.err ln "知らない指令: #%s" d))
      (scan base src);
    flush ();
    if !stack <> [] then Ast.err (base + 1) "#if が #endif で閉じていない"
  in
  file path src;
  let last = match !out with t :: _ -> t.ln | [] -> 0 in
  Array.of_list (List.rev ({ k = Eof; ln = last; sp = true; hide = [] } :: !out))
