(* C の構文解析。Cpp が出した字句の配列から、C の構文木を作る。
   型の名前（typedef）は読みながら覚え、構文木の型は typedef を展開した形にする。 *)

open Cpp

(* 型。整数は (符号付きか, バイト数)。構造体と共用体は名札で指し、中身は structs に入れる *)
type cty =
  | Void
  | Int of bool * int
  | Ptr of cty
  | Arr of cty * int option      (* 個数が書いてなければ None（初期化子で決まる） *)
  | Rec of string
  | Fn of cty * cty list

(* 構造体・共用体の中身。メンバーの名前が None なら無名のメンバー（中の名前がそのまま見える） *)
type record = { union : bool; mutable mems : (string option * cty) list; mutable full : bool }

let structs : (string, record) Hashtbl.t = Hashtbl.create 16

type expr = { e : desc; ln : int }

and desc =
  | Num of int * bool            (* 値, 符号なし（u が付く、または int に入らない 16 進） *)
  | Chr of int
  | Str of string
  | Id of string
  | Bin of string * expr * expr
  | Un of string * expr          (* - ~ ! * & + *)
  | Pre of string * expr         (* ++x --x *)
  | Post of string * expr        (* x++ x-- *)
  | Asg of string * expr * expr  (* = += -= ... *)
  | Call of expr * expr list
  | Index of expr * expr
  | Dot of expr * string
  | Arrow of expr * string
  | Cast of cty * expr
  | SizeT of cty
  | SizeE of expr
  | Cond of expr * expr * expr
  | Comma of expr * expr

type init = I of expr | L of init list

type storage = Auto | Static | Extern | Typedef

type decl = { name : string; ty : cty; st : storage; const : bool; init : init option; dln : int }

type stmt = { s : sdesc; sln : int }

and sdesc =
  | Expr of expr
  | Decl of decl list
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Do of stmt * expr
  | For of stmt option * expr option * expr option * stmt
  | Switch of expr * stmt
  | Case of expr
  | Default
  | Break
  | Continue
  | Return of expr option
  | AsmS of Cpp.tok list        (* __asm ... __endasm の中の字句（行の区切りは Nl） *)
  | Empty

type top =
  | TDecl of decl
  | TFun of { name : string; ret : cty; ps : (string * cty) list; body : stmt list; naked : bool; inline : bool;
              st : storage; fln : int }
  | TEnum of (string * expr option * int) list
  | TImport of string * int      (* #include "x.hl" *)

let err = Ast.err

(* 組み込みの型名。SDCC の char は符号なし *)
let builtin_types = [
  "uint8_t", Int (false, 1); "int8_t", Int (true, 1); "uint16_t", Int (false, 2); "int16_t", Int (true, 2);
  "bool", Int (false, 1); "_Bool", Int (false, 1); "size_t", Int (false, 2);
]

(* 無名の構造体に付ける名札の通し番号。structs と同じく、コンパイルの初めに reset で消す
   （ファイルをまたいで同じ名札にならないように） *)
let anon = ref 0
let reset () = Hashtbl.reset structs; anon := 0

let parse (ts : tok array) : top list =
  let types : (string, cty) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (k, v) -> Hashtbl.replace types k v) builtin_types;
  let i = ref 0 in
  let peek () = ts.(!i).k and ln () = ts.(!i).ln in
  let peek2 () = if !i + 1 < Array.length ts then ts.(!i + 1).k else Eof in
  let adv () = incr i in
  let fail what = err (ln ()) "%s が必要（%s がある）" what (match peek () with Eof -> "ファイルの終わり" | k -> spell k) in
  let p s = if peek () = P s then (adv (); true) else false in
  let need s = if not (p s) then fail s in
  let kw s = if peek () = Id s then (adv (); true) else false in
  let ident () = match peek () with Id s -> adv (); s | _ -> fail "名前" in
  let quals = [ "const"; "volatile"; "register"; "auto"; "inline"; "__inline"; "__naked"; "__critical"; "__reentrant"; "__nonbanked"; "__banked" ] in
  let type_words = [ "void"; "char"; "short"; "int"; "long"; "signed"; "unsigned"; "struct"; "union"; "enum"; "float"; "double" ] in
  let is_type_start () =
    match peek () with
    | Id s -> List.mem s type_words || List.mem s quals || Hashtbl.mem types s || List.mem s [ "static"; "extern"; "typedef" ]
    | _ -> false
  in
  let enums = ref [] in
  let last_params = ref [] in                          (* いちばん最近読んだ引数の並び（関数の定義で使う） *)

  (* ---- 式 ---- *)
  let num_of s =
    let l = String.lowercase_ascii s in
    let uns = String.contains l 'u' in
    let core = String.concat "" (String.split_on_char 'u' (String.concat "" (String.split_on_char 'l' l))) in
    let hexlike = String.length core > 1 && core.[0] = '0' in
    let core = if String.length core > 1 && core.[0] = '0' && core.[1] >= '0' && core.[1] <= '9' then "0o" ^ String.sub core 1 (String.length core - 1) else core in
    match int_of_string_opt core with
    | Some v -> v, uns || (hexlike && v > 0x7fff)
    | None -> err (ln ()) "数が読めない: %s" s
  in
  let rec expr () =
    let l = assign () in
    if peek () = P "," then (let n = ln () in adv (); { e = Comma (l, expr ()); ln = n }) else l
  and assign () =
    let l = cond () in
    match peek () with
    | P (("=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=") as o) ->
      let n = ln () in
      adv ();
      { e = Asg (o, l, assign ()); ln = n }
    | _ -> l
  and cond () =
    let c = bin 0 in
    if peek () = P "?" then begin
      let n = ln () in
      adv ();
      let a = expr () in
      need ":";
      { e = Cond (c, a, cond ()); ln = n }
    end else c
  and bin lv =
    let levels = [| [ "||" ]; [ "&&" ]; [ "|" ]; [ "^" ]; [ "&" ]; [ "=="; "!=" ]; [ "<"; ">"; "<="; ">=" ];
                    [ "<<"; ">>" ]; [ "+"; "-" ]; [ "*"; "/"; "%" ] |] in
    if lv = Array.length levels then cast ()
    else begin
      let l = ref (bin (lv + 1)) in
      let rec loop () =
        match peek () with
        | P o when List.mem o levels.(lv) ->
          let n = ln () in
          adv ();
          l := { e = Bin (o, !l, bin (lv + 1)); ln = n };
          loop ()
        | _ -> ()
      in
      loop ();
      !l
    end
  and cast () =
    if peek () = P "(" && (let j = !i in adv (); let t = is_type_start () in i := j; t) then begin
      let n = ln () in
      adv ();
      let t = type_name () in
      need ")";
      if peek () = P "{" then err n "複合リテラルは書けない";
      { e = Cast (t, cast ()); ln = n }
    end else unary ()
  and unary () =
    let n = ln () in
    match peek () with
    | P (("-" | "~" | "!" | "*" | "&" | "+") as o) -> adv (); { e = Un (o, cast ()); ln = n }
    | P (("++" | "--") as o) -> adv (); { e = Pre (o, unary ()); ln = n }
    | Id "sizeof" ->
      adv ();
      if peek () = P "(" && (let j = !i in adv (); let t = is_type_start () in i := j; t) then begin
        adv ();
        let t = type_name () in
        need ")";
        { e = SizeT t; ln = n }
      end else { e = SizeE (unary ()); ln = n }
    | _ -> postfix (primary ())
  and postfix e =
    let n = ln () in
    match peek () with
    | P "[" -> adv (); let x = expr () in need "]"; postfix { e = Index (e, x); ln = n }
    | P "(" ->
      adv ();
      let rec args acc = if p ")" then List.rev acc else (let a = assign () in if not (p ",") then (need ")"; List.rev (a :: acc)) else args (a :: acc)) in
      postfix { e = Call (e, args []); ln = n }
    | P "." -> adv (); postfix { e = Dot (e, ident ()); ln = n }
    | P "->" -> adv (); postfix { e = Arrow (e, ident ()); ln = n }
    | P (("++" | "--") as o) -> adv (); postfix { e = Post (o, e); ln = n }
    | _ -> e
  and primary () =
    let n = ln () in
    match peek () with
    | P "#" when (match peek2 () with Num _ -> true | _ -> false) -> adv (); primary ()   (* SDCC の #0x10 *)
    | Num s -> adv (); let v, u = num_of s in { e = Num (v, u); ln = n }
    | Cpp.Chr c -> adv (); { e = Chr c; ln = n }
    | Cpp.Str s ->
      adv ();
      let b = Buffer.create 16 in
      Buffer.add_string b s;
      let rec more () = match peek () with Cpp.Str s -> adv (); Buffer.add_string b s; more () | _ -> () in
      more ();
      { e = Str (Buffer.contents b); ln = n }
    | Id s -> adv (); { e = Id s; ln = n }
    | P "(" -> adv (); let e = expr () in need ")"; e
    | Asm _ -> err n "式の中にインラインアセンブラは書けない"
    | _ -> fail "式"

  (* ---- 型 ---- *)
  (* 宣言の頭: 記憶域、型。戻り値は (型, 記憶域, const か, naked か, inline か) *)
  and specs () =
    let st = ref Auto and const = ref false and naked = ref false and inl = ref false in
    let sign = ref None and size = ref None and base = ref None in
    let set_base t = if !base <> None then err (ln ()) "型が 2 つある" else base := Some t in
    let rec loop () =
      match peek () with
      | Id "typedef" -> adv (); st := Typedef; loop ()
      | Id "static" -> adv (); st := Static; loop ()
      | Id "extern" -> adv (); st := Extern; loop ()
      | Id "const" -> adv (); const := true; loop ()
      | Id ("__naked") -> adv (); naked := true; loop ()
      | Id ("inline" | "__inline") -> adv (); inl := true; loop ()
      | Id q when List.mem q quals -> adv (); loop ()
      | Id ("__at" | "__sfr" | "__interrupt") as k -> err (ln ()) "%s は未対応" (spell k)
      | Id "signed" -> adv (); sign := Some true; loop ()
      | Id "unsigned" -> adv (); sign := Some false; loop ()
      | Id "char" -> adv (); size := Some 1; loop ()
      | Id "short" -> adv (); size := Some 2; loop ()
      | Id "int" -> adv (); (if !size = None then size := Some 2); loop ()
      | Id "long" -> err (ln ()) "long（32bit）は未対応"
      | Id ("float" | "double") -> err (ln ()) "浮動小数点は未対応"
      | Id "void" -> adv (); set_base Void; loop ()
      | Id (("struct" | "union") as k) -> adv (); set_base (record (k = "union")); loop ()
      | Id "enum" -> adv (); set_base (enum ()); loop ()
      | Id s when Hashtbl.mem types s && !base = None && !sign = None && !size = None -> adv (); base := Some (Hashtbl.find types s); loop ()
      | _ -> ()
    in
    loop ();
    let t =
      match !base, !sign, !size with
      | Some t, None, None -> t
      | Some _, _, _ -> err (ln ()) "型の書き方がおかしい"
      | None, None, None -> Int (true, 2)                     (* 型を書かなければ int *)
      | None, s, Some 1 -> Int (Option.value s ~default:false, 1)   (* char は符号なし（SDCC） *)
      | None, s, sz -> Int (Option.value s ~default:true, Option.value sz ~default:2)
    in
    t, !st, !const, !naked, !inl
  and record union =
    let tag = match peek () with Id s -> adv (); s | _ -> incr anon; Printf.sprintf "anon.%d" !anon in
    if not (Hashtbl.mem structs tag) then Hashtbl.replace structs tag { union; mems = []; full = false };
    let r = Hashtbl.find structs tag in
    if p "{" then begin
      let mems = ref [] in
      while not (p "}") do
        let t, _, _, _, _ = specs () in
        if p ";" then
          (match t with Rec _ -> mems := (None, t) :: !mems | _ -> err (ln ()) "名前のないメンバー")
        else begin
          let rec each () =
            let name, w = declarator () in
            if peek () = P ":" then err (ln ()) "ビットフィールドは未対応";
            (match name with Some n -> mems := (Some n, w t) :: !mems | None -> err (ln ()) "メンバーの名前がない");
            if p "," then each () else need ";"
          in
          each ()
        end
      done;
      r.mems <- List.rev !mems;
      r.full <- true
    end;
    Rec tag
  and enum () =
    (match peek () with Id _ -> adv () | _ -> ());
    if p "{" then begin
      let rec each acc =
        if p "}" then List.rev acc
        else begin
          let n = ln () in
          let name = ident () in
          let v = if p "=" then Some (cond ()) else None in
          let acc = (name, v, n) :: acc in
          if p "," then each acc else (need "}"; List.rev acc)
        end
      in
      enums := TEnum (each []) :: !enums
    end;
    Int (true, 2)
  (* 宣言子: 名前と、頭の型から全体の型を作る関数 *)
  and declarator () =
    let rec stars () = if p "*" then (while List.exists kw [ "const"; "volatile" ] do () done; 1 + stars ()) else 0 in
    let n = stars () in
    let name, inner =
      match peek () with
      | Id s when not (List.mem s quals) -> adv (); Some s, Fun.id
      | P "(" when peek2 () = P "*" -> adv (); let r = declarator () in need ")"; r
      | _ -> None, Fun.id
    in
    let rec suffixes () =
      if p "[" then begin
        let size = if peek () = P "]" then None else Some (cond ()) in
        need "]";
        let rest = suffixes () in
        fun t -> Arr (rest t, Option.map (fun e -> match const_int e with Some v -> v | None -> err e.ln "配列の大きさは定数") size)
      end
      else if peek () = P "(" then begin
        adv ();
        let ps = params () in
        last_params := ps;
        let rest = suffixes () in
        fun t -> Fn (rest t, List.map snd ps)
      end
      else Fun.id
    in
    let suf = suffixes () in
    let rec ptr t k = if k = 0 then t else ptr (Ptr t) (k - 1) in
    name, fun t -> inner (suf (ptr t n))
  and params () =
    (* 関数の引数。( void ) は引数なし *)
    if p ")" then []
    else if peek () = Id "void" && peek2 () = P ")" then (adv (); adv (); [])
    else begin
      let rec each acc =
        if peek () = P "..." then err (ln ()) "可変長引数は未対応";
        let t, _, _, _, _ = specs () in
        let name, w = declarator () in
        let t = match w t with Arr (e, _) -> Ptr e | t -> t in        (* 配列の引数はポインタ *)
        let acc = (Option.value name ~default:"", t) :: acc in
        if p "," then each acc else (need ")"; List.rev acc)
      in
      each []
    end
  and type_name () =
    let t, _, _, _, _ = specs () in
    let _, w = declarator () in
    w t
  (* 定数式（配列の大きさ）。ここでは数と簡単な演算だけ *)
  and const_int e =
    match e.e with
    | Num (v, _) | Chr v -> Some v
    | Bin (o, a, b) ->
      (match const_int a, const_int b with
       | Some a, Some b ->
         (match o with
          | "+" -> Some (a + b) | "-" -> Some (a - b) | "*" -> Some (a * b)
          | "/" when b <> 0 -> Some (a / b) | "<<" -> Some (a lsl b) | ">>" -> Some (a asr b)
          | "|" -> Some (a lor b) | "&" -> Some (a land b) | _ -> None)
       | _ -> None)
    | Un ("-", a) -> Option.map (fun v -> -v) (const_int a)
    | SizeT t -> Some (sizeof t)
    | _ -> None
  and sizeof t =
    match t with
    | Void -> 1
    | Int (_, n) -> n
    | Ptr _ | Fn _ -> 2
    | Arr (t, Some n) -> n * sizeof t
    | Arr (_, None) -> 0
    | Rec tag ->
      let r = Hashtbl.find structs tag in
      if not r.full then err (ln ()) "中身の分からない構造体: %s" tag;
      let sizes = List.map (fun (_, t) -> sizeof t) r.mems in
      if r.union then List.fold_left max 0 sizes else List.fold_left ( + ) 0 sizes
  in

  (* ---- 初期化子 ---- *)
  let rec init () =
    if p "{" then begin
      let rec each acc =
        if p "}" then List.rev acc
        else begin
          if peek () = P "." || peek () = P "[" then err (ln ()) "指示付きの初期化子は未対応";
          let x = init () in
          if p "," then each (x :: acc) else (need "}"; List.rev (x :: acc))
        end
      in
      L (each [])
    end else I (assign ())
  in

  (* ---- 文 ---- *)
  let rec stmt () =
    let n = ln () in
    let mk s = { s; sln = n } in
    match peek () with
    | P "{" -> adv (); mk (Block (block ()))
    | P ";" -> adv (); mk Empty
    | Id "if" ->
      adv (); need "(";
      let c = expr () in
      need ")";
      let t = stmt () in
      let f = if kw "else" then Some (stmt ()) else None in
      mk (If (c, t, f))
    | Id "while" -> adv (); need "("; let c = expr () in need ")"; mk (While (c, stmt ()))
    | Id "do" ->
      adv ();
      let b = stmt () in
      if not (kw "while") then fail "while";
      need "(";
      let c = expr () in
      need ")"; need ";";
      mk (Do (b, c))
    | Id "for" ->
      adv (); need "(";
      let init =
        if p ";" then None
        else if is_type_start () then Some (mk (Decl (decls ())))
        else (let e = expr () in need ";"; Some (mk (Expr e)))
      in
      let c = if peek () = P ";" then None else Some (expr ()) in
      need ";";
      let step = if peek () = P ")" then None else Some (expr ()) in
      need ")";
      mk (For (init, c, step, stmt ()))
    | Id "switch" -> adv (); need "("; let e = expr () in need ")"; mk (Switch (e, stmt ()))
    | Id "case" -> adv (); let e = cond () in need ":"; mk (Case e)
    | Id "default" -> adv (); need ":"; mk Default
    | Id "break" -> adv (); need ";"; mk Break
    | Id "continue" -> adv (); need ";"; mk Continue
    | Id "return" -> adv (); if p ";" then mk (Return None) else (let e = expr () in need ";"; mk (Return (Some e)))
    | Id "goto" -> err n "goto は未対応"
    | Asm s -> adv (); ignore (p ";"); mk (AsmS s)
    | Id _ when peek2 () = P ":" && not (is_type_start ()) -> err n "ラベルは未対応"
    | _ when is_type_start () -> mk (Decl (decls ()))
    | _ -> let e = expr () in need ";"; mk (Expr e)
  and block () = if p "}" then [] else (let s = stmt () in s :: block ())
  (* ブロックの中の宣言。int a = 1, *b; *)
  and decls () =
    let t, st, const, _, _ = specs () in
    if p ";" then []
    else begin
      let rec each acc =
        let n = ln () in
        let name, w = declarator () in
        let name = match name with Some x -> x | None -> err n "宣言の名前がない" in
        let ty = w t in
        let init = if p "=" then Some (init ()) else None in
        if st = Typedef then Hashtbl.replace types name ty;
        let acc = { name; ty; st; const; init; dln = n } :: acc in
        if p "," then each acc else (need ";"; List.rev acc)
      in
      each []
    end
  in

  (* ---- ファイル全体 ---- *)
  let tops = ref [] in
  let add t = tops := t :: !tops in
  while peek () <> Eof do
    let n = ln () in
    if p ";" then ()
    else if kw "__hl_import" then (match peek () with Cpp.Str f -> adv (); need ";"; add (TImport (f, n)) | _ -> fail "ファイル名")
    else begin
      let t, st, const, naked, inl = specs () in
      List.iter add (List.rev !enums);
      enums := [];
      if p ";" then ()
      else begin
        let rec each first =
          let n = ln () in
          let name, w = declarator () in
          let name = match name with Some x -> x | None -> err n "宣言の名前がない" in
          let ty = w t in
          let naked = kw "__naked" || naked in
          match ty with
          | Fn (ret, _) when first && peek () = P "{" ->
            (* 関数の定義。引数の名前は宣言子からもう一度読む *)
            let ps = !last_params in
            adv ();
            let body = block () in
            add (TFun { name; ret; ps; body; naked; inline = inl; st; fln = n })
          | _ ->
            let init = if p "=" then Some (init ()) else None in
            if st = Typedef then Hashtbl.replace types name ty
            else add (TDecl { name; ty; st; const; init; dln = n });
            List.iter add (List.rev !enums);
            enums := [];
            if p "," then each false else need ";"
        in
        each true
      end
    end
  done;
  List.rev !tops
