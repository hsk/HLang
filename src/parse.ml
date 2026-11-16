(* 構文解析: 再帰下降。式は優先順位の表を下から上へたどる。 *)

open Ast
open Lex

let parse ?base src =
  let toks = lex ?base src in
  let i = ref 0 in
  let peek () = fst toks.(!i) in
  let line () = snd toks.(!i) in
  let next () = let t = peek () in if t <> Eof then incr i; t in
  let fail what = err (line ()) "%s が必要" what in
  let eat p = if peek () = P p then (incr i; true) else false in
  let kw k = if peek () = Id k then (incr i; true) else false in
  let expect p = if not (eat p) then fail p in
  let ident () = match next () with Id s -> s | _ -> decr i; fail "名前" in
  let mk l x = { x; a = l } in

  let ty () =
    if eat "*" then Ptr (ident ()) else
    match ident () with
    | "u8" -> U8
    | "u16" -> U16
    | "i8" -> I8
    | "i16" -> I16
    | "bool" -> Bool
    | s -> Enum s                              (* 本当にあるかは型検査で調べる *)
  in

  (* 優先順位の低い順。同じ段は左結合。 *)
  let levels = [
    [ "||" ]; [ "&&" ]; [ "=="; "!="; "<"; "<="; ">"; ">=" ];
    [ "|" ]; [ "^" ]; [ "&" ]; [ "<<"; ">>" ]; [ "+"; "-" ]; [ "*"; "/"; "%" ];
  ] in

  let rec expr () = binary levels
  and binary = function
    | [] -> postfix (unary ())                 (* as は単項演算子より弱い: !x as u8 *)
    | ops :: rest ->
      let l = ref (binary rest) in
      let rec loop () =
        match peek () with
        | P p when List.mem p ops ->
          let ln = line () in
          incr i;
          l := mk ln (Bin (p, !l, binary rest));
          loop ()
        | _ -> ()
      in
      loop ();
      !l
  and unary () =
    let ln = line () in
    match peek () with
    | P (("-" | "~" | "!") as p) -> incr i; mk ln (Un (p, unary ()))
    | _ -> primary ()
  and postfix e =
    let ln = line () in
    if kw "as" then postfix (mk ln (Cast (ty (), e))) else e
  and suffix e =                               (* 表[添字] *)
    let ln = line () in
    if peek () = P "[" then suffix (mk ln (Index (e, index ())))
    else if eat "." then suffix (mk ln (Field (e, ident ())))
    else e
  and index () = expect "["; let e = expr () in expect "]"; e
  and primary () =
    let ln = line () in
    match next () with
    | Int n -> mk ln (Int n)
    | Str s -> mk ln (Str s)
    | Id "true" -> mk ln (Tf true)
    | Id "false" -> mk ln (Tf false)
    | Id "mem" -> mk ln (Mem (U8, index ()))
    | Id "mem16" -> mk ln (Mem (U16, index ()))
    | Id "port" -> mk ln (Port (index ()))
    | Id "spawn" -> let t = ident () in expect "("; mk ln (Spawn (t, args ")"))
    | Id f when peek () = P "(" -> incr i; mk ln (Call (f, args ")"))
    | Id v -> suffix (mk ln (Var v))
    | P "(" -> let e = expr () in expect ")"; e
    | P "&" -> mk ln (Addr (ident ()))         (* &名前: 関数や表の番地 *)
    | _ -> decr i; fail "式"
  and args close =                             (* 閉じ記号まで、カンマ区切り *)
    if eat close then []
    else begin
      let e = expr () in
      if eat "," then e :: args close else (expect close; [ e ])
    end
  in

  let rec block () =
    expect "{";
    let rec go () = if eat "}" then [] else (let s = stmt () in ignore (eat ";"); s :: go ()) in
    go ()
  and stmt () =
    let ln = line () in
    if kw "var" then begin
      let x = ident () in
      let t = if eat ":" then Some (ty ()) else None in
      expect "=";
      Let (x, t, expr ())
    end
    else if kw "if" then if_ ()
    else if kw "while" then (let c = expr () in While (c, block ()))
    else if kw "loop" then While (mk ln (Tf true), block ())
    else if kw "for" then begin
      let x = ident () in
      if not (kw "in") then fail "in";
      let lo = expr () in
      (match lo.x with
       | Var t when peek () = P "{" -> Each (x, t, block ())      (* for e in タスク { ... } *)
       | _ ->
         expect "..";
         let hi = expr () in
         For (x, lo, hi, block ()))
    end
    else if kw "match" then begin
      let e = expr () in
      expect "{";
      let arms = ref [] and other = ref None in
      while not (eat "}") do
        if peek () = Id "_" then begin
          incr i;
          expect "=>";
          other := Some (block ())
        end else begin
          let rec pats () = let p = expr () in if eat "," then p :: pats () else [ p ] in
          let ps = pats () in
          expect "=>";
          arms := (ps, block ()) :: !arms
        end;
        ignore (eat ",")
      done;
      Match (e, List.rev !arms, !other)
    end
    else if kw "yield" then Yield
    else if kw "wait" then Wait (expr ())
    else if kw "resume" then Resume (ident ())
    else if kw "stop" then Stop ("", Some (expr ()))
    else if kw "break" then Break
    else if kw "continue" then Continue
    else if kw "return" then Return (if peek () = P "}" || peek () = P ";" then None else Some (expr ()))
    else begin
      let e = expr () in
      match peek () with
      | P "=" -> incr i; Set (e, expr ())
      | P (("+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=") as p) ->
        incr i;
        let op = String.sub p 0 (String.length p - 1) in
        Set (e, mk ln (Bin (op, e, expr ())))
      | _ -> Do e
    end
  and if_ () =
    let c = expr () in
    let t = block () in
    let f = if kw "else" then (if kw "if" then [ if_ () ] else block ()) else [] in
    If (c, t, f)
  in

  let rec params () =                          (* 名前: 型, ... ) *)
    if eat ")" then []
    else begin
      let x = ident () in
      expect ":";
      let p = x, ty () in
      if eat "," then p :: params () else (expect ")"; [ p ])
    end
  in
  let top () =
    let ln = line () in
    if kw "const" then (let x = ident () in expect "="; Const (x, expr ()))
    else if kw "var" then begin
      let x = ident () in
      expect ":";
      if eat "[" then begin                    (* var buf: [u8; 16] *)
        let t = ty () in
        expect ";";
        let n = expr () in
        expect "]";
        Array (x, t, n, ln)
      end else begin
        let t = ty () in
        Global (x, t, if eat "=" then Some (expr ()) else None)
      end
    end
    else if kw "data" then begin               (* data tbl: u8 = [1, 2, 3] か = "文字列" *)
      let x = ident () in
      expect ":";
      let t = ty () in
      expect "=";
      match peek () with
      | Str s ->
        incr i;
        Data (x, t, List.map (fun c -> mk ln (Int (Char.code c))) (List.of_seq (String.to_seq s)), ln)
      | _ -> expect "["; Data (x, t, args "]", ln)
    end
    else if kw "enum" then begin               (* enum Op { A, B = 5, C } *)
      let name = ident () in
      expect "{";
      let rec vs () =
        if eat "}" then []
        else begin
          let v = ident () in
          let e = if eat "=" then Some (expr ()) else None in
          ignore (eat ",");
          (v, e) :: vs ()
        end
      in
      EnumDef (name, vs (), ln)
    end
    else if kw "extern" then begin
      if not (kw "fn") then fail "fn";
      let name = ident () in
      expect "(";
      let rec ps () =
        if eat ")" then []
        else begin
          ignore (ident ());
          expect ":";
          let t = ty () in
          expect "@";
          let p = t, ident () in
          if eat "," then p :: ps () else (expect ")"; [ p ])
        end
      in
      let ps = ps () in
      let ret = if eat "->" then ty () else Unit in
      expect "@";
      let addr = expr () in
      let clob =
        if kw "clobbers" then begin
          expect "(";
          let rec rs () = if eat ")" then [] else (let r = ident () in ignore (eat ","); r :: rs ()) in
          Some (rs ())
        end else None
      in
      Extern (name, ps, ret, addr, clob, ln)
    end
    else if kw "struct" then begin             (* struct Enemy { x: u8, hp: u8 } *)
      let name = ident () in
      expect "{";
      let rec fs () =
        if eat "}" then []
        else begin
          let f = ident () in
          expect ":";
          let t = ty () in
          ignore (eat ",");
          (f, t) :: fs ()
        end
      in
      StructDef (name, fs (), ln)
    end
    else if peek () = Id "fn" || peek () = Id "lut" then begin
      let lut = kw "lut" in
      if not (kw "fn") then fail "fn";
      let name = ident () in
      expect "(";
      let ps = params () in
      let ret = if eat "->" then ty () else Unit in
      Fn ({ name; ps; ret; body = block (); lut; task = false; pool = 0; self = "" }, ln)
    end
    else if kw "task" then begin               (* task 名前() { ... } / task 名前(x: u8) pool 8 { ... } *)
      let name = ident () in
      expect "(";
      let ps = params () in
      let pool = if kw "pool" then (match next () with Int n when n > 0 -> n | _ -> decr i; fail "個数") else 0 in
      if ps <> [] && pool = 0 then err ln "引数を取れるのは pool を付けたタスクだけ";
      Fn ({ name; ps; ret = Unit; body = block (); lut = false; task = true; pool; self = "" }, ln)
    end
    else if kw "import" then (match next () with Str s -> Import (s, ln) | _ -> decr i; fail "ファイル名")
    else fail "fn, task, var, const, data, enum, struct, extern, import のどれか"
  in
  let rec tops () = if peek () = Eof then [] else (let t = top () in ignore (eat ";"); t :: tops ()) in
  tops ()
