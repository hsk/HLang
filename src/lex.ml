(* 字句解析: 文字列を (字句, 行番号) の配列にする。 *)

type tok =
  | Id of string
  | Int of int
  | Str of string
  | P of string                  (* 記号 *)
  | Eof

let puncts = [
  "<<="; ">>="; "=>"; "=="; "!="; "<="; ">="; "&&"; "||"; "<<"; ">>"; "->"; ".."; ".";
  "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^=";
  "+"; "-"; "*"; "/"; "%"; "&"; "|"; "^"; "~"; "!"; "<"; ">"; "="; "("; ")"; "{"; "}";
  "["; "]"; ","; ":"; ";"; "@";
]

let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let lex ?(base = 0) s =
  let n = String.length s in
  let line = ref (base + 1) and i = ref 0 and out = ref [] in
  let push t = out := (t, !line) :: !out in
  let span p =                                 (* p が成り立つ間の部分文字列 *)
    let j = ref !i in
    while !j < n && p s.[!j] do incr j done;
    let r = String.sub s !i (!j - !i) in
    i := !j;
    r
  in
  let esc () =                                 (* 文字 1 つ。\n などを読む *)
    let c = s.[!i] in
    incr i;
    if c <> '\\' then c
    else begin
      let e = s.[!i] in
      incr i;
      match e with 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t' | '0' -> '\000' | c -> c
    end
  in
  while !i < n do
    let c = s.[!i] in
    if c = '\n' then (incr line; incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '/' && !i + 1 < n && s.[!i + 1] = '/' then ignore (span (fun c -> c <> '\n'))
    else if is_digit c then begin
      let t = span is_id in
      match int_of_string_opt t with
      | Some v -> push (Int v)
      | None -> Ast.err !line "数値として読めない: %s" t
    end
    else if is_id c then push (Id (span is_id))
    else if c = '\'' then begin
      incr i;
      let v = esc () in
      if !i >= n || s.[!i] <> '\'' then Ast.err !line "文字定数が閉じていない";
      incr i;
      push (Int (Char.code v))
    end
    else if c = '"' then begin
      incr i;
      let b = Buffer.create 16 in
      while !i < n && s.[!i] <> '"' do Buffer.add_char b (esc ()) done;
      if !i >= n then Ast.err !line "文字列が閉じていない";
      incr i;
      push (Str (Buffer.contents b))
    end
    else begin
      let fits p = !i + String.length p <= n && String.sub s !i (String.length p) = p in
      match List.find_opt fits puncts with
      | Some p -> i := !i + String.length p; push (P p)
      | None -> Ast.err !line "読めない文字: %c" c
    end
  done;
  push Eof;
  Array.of_list (List.rev !out)
