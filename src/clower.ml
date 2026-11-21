(* C の構文木を、C の意味を保ったまま hlang の構文木に直す。
   - 整数の格上げは C のとおり（u8 どうしの足し算も int で計算する）。ただし結果の下位 8 ビットしか使わない
     所（u8 への代入、(u8) のキャスト、0〜255 に収まると分かる値の比較）では、初めから 8 ビットで計算する式を出す。
   - 構造体と配列は、バイトの位置を自分で計算して mem[番地 + 位置] にする。
   - 番地を取られる変数、局所の配列と構造体は、固定の番地に置く（hlang には再帰がないので、それで正しい）。 *)

open Cparse
module A = Ast

let err = Ast.err

(* 値の置き場所。V は hlang の変数（スカラー）、M はラベルの番地にあるもの、K は定数 *)
type place = V of string * cty | M of string * cty | K of int * cty

(* hlang のライブラリ（#include "x.hl"）の関数: 引数の型と戻りの型（hlang の型） *)
type hsig = A.ty list * A.ty

type env = {
  globals : (string, place) Hashtbl.t;
  funs : (string, cty * cty list) Hashtbl.t;
  hfuns : (string, hsig) Hashtbl.t;
  enums : (string, int) Hashtbl.t;
  taken : (string, unit) Hashtbl.t;               (* & で番地を取られる名前 *)
  strs : (string, string) Hashtbl.t;              (* 文字列 -> 表の名前 *)
  mutable tops : int A.top list;                  (* 作った大域（逆順） *)
  mutable inits : int A.stmt list;                (* 起動時の初期化（逆順） *)
  mutable scope : (string * place) list;
  mutable used : string list;                     (* この関数で使った hlang の名前 *)
  mutable pre : int A.stmt list;                  (* いまの文の前に出す文（逆順） *)
  mutable fname : string;
  mutable ret : cty;
  mutable n : int;
  mutable conts : int A.stmt list list;           (* continue の前に出す文（for の step など）。内側が先頭 *)
  mutable brk : [ `Loop | `Switch ] list;
  mutable tu : int;                               (* いま見ている .c ファイルの番号 *)
  ren : (int * string, string) Hashtbl.t;         (* (ファイル, static の名前) -> 付け直した名前 *)
}

let mk ln x : int A.expr = { A.x; a = ln }
let int ln n = mk ln (A.Int n)
let u8t = Int (false, 1) and i8t = Int (true, 1) and u16t = Int (false, 2) and i16t = Int (true, 2)

(* ---- 型 ---- *)

let rec size ln = function
  | Void -> 1
  | Int (_, n) -> n
  | Ptr _ | Fn _ -> 2
  | Arr (t, Some n) -> n * size ln t
  | Arr (_, None) -> err ln "大きさの分からない配列"
  | Rec tag ->
    let r = Hashtbl.find structs tag in
    if not r.full then err ln "中身の分からない構造体: %s" tag;
    let ss = List.map (fun (_, t) -> size ln t) r.mems in
    if r.union then List.fold_left max 0 ss else List.fold_left ( + ) 0 ss

(* メンバーの位置と型。無名のメンバーの中も探す *)
let rec member ln tag f =
  let r = Hashtbl.find structs tag in
  let rec go off = function
    | [] -> None
    | (name, t) :: rest ->
      let here = if r.union then 0 else off in
      let found =
        match name, t with
        | Some n, _ when n = f -> Some (here, t)
        | None, Rec t2 -> Option.map (fun (o, t) -> here + o, t) (member ln t2 f)
        | _ -> None
      in
      (match found with Some x -> Some x | None -> go (off + size ln t) rest)
  in
  go 0 r.mems

let field ln tag f = match member ln tag f with Some x -> x | None -> err ln "%s に %s というメンバーはない" tag f

let hl ln = function
  | Int (false, 1) -> A.U8
  | Int (true, 1) -> A.I8
  | Int (false, _) -> A.U16
  | Int (true, _) -> A.I16
  | Ptr _ | Arr _ | Fn _ -> A.U16
  | Void -> A.Unit
  | Rec t -> err ln "構造体 %s は値として使えない" t

let of_hl = function
  | A.U8 | A.Bool | A.Enum _ -> u8t
  | A.I8 -> i8t
  | A.U16 -> u16t
  | A.I16 -> i16t
  | A.Ptr _ -> Ptr Void
  | A.Unit -> Void

let decay = function Arr (t, _) -> Ptr t | Fn _ -> Ptr Void | t -> t
let is_int = function Int _ -> true | _ -> false
let is_ptr = function Ptr _ | Arr _ | Fn _ -> true | _ -> false
let promote = function Int (_, 1) -> i16t | t -> t
let usual a b = match promote a, promote b with Int (false, 2), _ | _, Int (false, 2) -> u16t | _ -> i16t

let elem ln = function
  | Ptr t | Arr (t, _) -> t
  | t -> ignore t; err ln "ポインタでも配列でもない"

let lit_ty n u = if u || n > 0x7fff then u16t else i16t

(* ---- 名前 ---- *)

(* 大域の名前。ほかのファイルとぶつかる static は、付け直した名前になる *)
let rn env x = Option.value (Hashtbl.find_opt env.ren (env.tu, x)) ~default:x

let lookup env (_ : int) x =
  match List.assoc_opt x env.scope, Hashtbl.find_opt env.globals (rn env x) with
  | Some p, _ | None, Some p -> Some p
  | None, None -> Option.map (fun v -> K (v, i16t)) (Hashtbl.find_opt env.enums x)

let fresh env base =
  let rec go k =
    let n = if k = 0 then base else Printf.sprintf "%s_%d" base k in
    if List.mem n env.used || Hashtbl.mem env.globals n then go (k + 1) else n
  in
  let n = go 0 in
  env.used <- n :: env.used;
  n

let tmp env = env.n <- env.n + 1; Printf.sprintf "t.%d" env.n
let emit env s = env.pre <- s :: env.pre
let collect env f =
  let saved = env.pre in
  env.pre <- [];
  let r = f () in
  let p = List.rev env.pre in
  env.pre <- saved;
  p, r

(* ---- 定数式 ---- *)

let rec cval env (e : expr) =
  let c = cval env in
  match e.e with
  | Num (v, _) | Chr v -> Some v
  | Id x -> (match lookup env e.ln x with Some (K (v, _)) -> Some v | _ -> None)
  | Un ("-", a) -> Option.map (fun v -> -v) (c a)
  | Un ("~", a) -> Option.map lnot (c a)
  | Un ("+", a) -> c a
  | Un ("!", a) -> Option.map (fun v -> if v = 0 then 1 else 0) (c a)
  | Bin (o, a, b) ->
    (match c a, c b with
     | Some a, Some b ->
       let bool x = Some (if x then 1 else 0) in
       (match o with
        | "+" -> Some (a + b) | "-" -> Some (a - b) | "*" -> Some (a * b)
        | "/" -> if b = 0 then None else Some (a / b) | "%" -> if b = 0 then None else Some (a mod b)
        | "<<" -> Some (a lsl b) | ">>" -> Some (a asr b)
        | "&" -> Some (a land b) | "|" -> Some (a lor b) | "^" -> Some (a lxor b)
        | "==" -> bool (a = b) | "!=" -> bool (a <> b) | "<" -> bool (a < b) | ">" -> bool (a > b)
        | "<=" -> bool (a <= b) | ">=" -> bool (a >= b) | "&&" -> bool (a <> 0 && b <> 0) | "||" -> bool (a <> 0 || b <> 0)
        | _ -> None)
     | _ -> None)
  | Cast (Int (s, n), a) ->
    Option.map (fun v -> let m = if n = 1 then 0xff else 0xffff in let v = v land m in if s && v > m / 2 then v - m - 1 else v) (c a)
  | SizeT t -> Some (size e.ln t)
  | SizeE x -> Some (size e.ln (ty_lv env x))
  | Cond (a, b, d) -> (match c a with Some 0 -> c d | Some _ -> c b | None -> None)
  | _ -> None

(* ---- 式の型（値として使うとき。配列は先頭へのポインタになる） ---- *)

and ty_lv env (e : expr) =
  match e.e with
  | Id x ->
    (match lookup env e.ln x with
     | Some (V (_, t) | M (_, t) | K (_, t)) -> t
     | None ->
       match Hashtbl.find_opt env.funs (rn env x), Hashtbl.find_opt env.hfuns x with
       | Some (r, ps), _ -> Fn (r, ps)
       | None, Some _ -> Fn (Void, [])
       | None, None -> err e.ln "知らない名前: %s" x)
  | Un ("*", p) -> elem e.ln (ty env p)
  | Index (a, _) -> elem e.ln (ty env a)
  | Dot (s, f) -> (match ty_lv env s with Rec tag -> snd (field e.ln tag f) | _ -> err e.ln ". の左が構造体でない")
  | Arrow (p, f) -> (match ty env p with Ptr (Rec tag) -> snd (field e.ln tag f) | _ -> err e.ln "-> の左が構造体のポインタでない")
  | _ -> ty env e

and ty env (e : expr) =
  match e.e with
  | Num (n, u) -> lit_ty n u
  | Chr _ -> i16t
  | Str _ -> Ptr u8t
  | Id _ | Un ("*", _) | Index _ | Dot _ | Arrow _ -> decay (ty_lv env e)
  | Bin (("==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"), _, _) | Un ("!", _) -> i16t
  | Bin (("+" | "-") as o, a, b) ->
    (match ty env a, ty env b with
     | (Ptr _ as p), t when is_int t -> p
     | t, (Ptr _ as p) when is_int t && o = "+" -> p
     | Ptr _, Ptr _ -> i16t
     | x, y -> usual x y)
  | Bin (("<<" | ">>"), a, _) -> promote (ty env a)
  | Bin (_, a, b) -> usual (ty env a) (ty env b)
  | Un ("&", a) -> Ptr (ty_lv env a)
  | Un (_, a) -> promote (ty env a)
  | Pre (_, a) | Post (_, a) | Asg (_, a, _) -> ty_lv env a
  | Call ({ e = Id f; _ }, _) ->
    (match Hashtbl.find_opt env.funs (rn env f), Hashtbl.find_opt env.hfuns f with
     | Some (r, _), _ -> r
     | None, Some (_, r) -> of_hl r
     | None, None -> err e.ln "知らない関数: %s" f)
  | Call _ -> err e.ln "関数ポインタでの呼び出しは未対応"
  | Cast (t, _) -> t
  | SizeT _ | SizeE _ -> u16t
  | Cond (_, a, b) -> (match ty env a, ty env b with (Ptr _ as p), _ | _, (Ptr _ as p) -> p | x, y -> usual x y)
  | Comma (_, b) -> ty env b

(* 値が必ず 0〜255 に入るか（下位 8 ビットだけで計算・比較してよいか） *)
let rec fits8 env (e : expr) =
  match cval env e with
  | Some n -> n >= 0 && n <= 255
  | None ->
  match e.e with
  | Num (n, _) | Chr n -> n >= 0 && n <= 255
  | Id _ | Index _ | Dot _ | Arrow _ | Un ("*", _) -> (match cval env e with Some n -> n >= 0 && n <= 255 | None -> ty_lv env e = u8t)
  | Cast (Int (false, 1), _) -> true
  | Cast (Int (false, 2), a) -> fits8 env a
  | Bin ("&", a, b) -> fits8 env a || fits8 env b
  | Bin (("|" | "^"), a, b) -> fits8 env a && fits8 env b
  | Bin (">>", a, _) -> fits8 env a
  | Bin (("==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"), _, _) | Un ("!", _) -> true
  | Cond (_, a, b) -> fits8 env a && fits8 env b
  | Call _ | Pre _ | Post _ | Asg _ -> ty env e = u8t
  | _ -> false

let rec fitsi8 env (e : expr) =
  match cval env e with
  | Some n -> n >= -128 && n <= 127
  | None ->
  match e.e with
  | Num (n, _) | Chr n -> n <= 127
  | Un ("-", a) -> (match cval env a with Some n -> n <= 128 | None -> false)
  | Id _ | Index _ | Dot _ | Arrow _ | Un ("*", _) -> (match cval env e with Some n -> n >= -128 && n <= 127 | None -> ty_lv env e = i8t)
  | Cast (Int (true, 1), _) -> true
  | Cond (_, a, b) -> fitsi8 env a && fitsi8 env b
  | _ -> false

(* 式に副作用（代入、++、関数の呼び出し）があるか *)
let rec effects (e : expr) =
  match e.e with
  | Pre _ | Post _ | Asg _ | Call _ -> true
  | Num _ | Chr _ | Str _ | Id _ | SizeT _ | SizeE _ -> false
  | Bin (_, a, b) | Index (a, b) | Comma (a, b) -> effects a || effects b
  | Un (_, a) | Dot (a, _) | Arrow (a, _) | Cast (_, a) -> effects a
  | Cond (a, b, c) -> effects a || effects b || effects c

(* ---- 文字列と、表 ---- *)

let str env ln s =
  match Hashtbl.find_opt env.strs s with
  | Some l -> l
  | None ->
    let l = Printf.sprintf "cstr.%d" (Hashtbl.length env.strs) in
    Hashtbl.replace env.strs s l;
    let bytes = List.map (fun c -> int ln (Char.code c)) (List.of_seq (String.to_seq s)) @ [ int ln 0 ] in
    env.tops <- A.Data (l, A.U8, bytes, ln) :: env.tops;
    l

(* ---- 式を直す ---- *)

type v = { x : int A.expr; t : cty }

(* 置き場所: 変数（hlang の変数）か、番地 *)
type lval = LV of string * cty | LM of int A.expr * cty

let add ln a off = if off = 0 then a else mk ln (A.Bin ("+", a, int ln (off land 0xffff)))

let scale ln x s =
  let rec log2 n k = if n = 1 then Some k else if n land 1 = 1 then None else log2 (n lsr 1) (k + 1) in
  match log2 s 0 with
  | Some 0 -> x
  | Some k -> mk ln (A.Bin ("<<", x, int ln k))
  | None -> mk ln (A.Bin ("*", x, int ln s))

(* v を C の型 t に変える *)
let conv ln (v : v) t =
  if t = Void then err ln "void へは変換できない";
  match v.x.x with
  | A.Int n ->
    let m = if size ln t = 1 then 0xff else 0xffff in
    (* 元が符号付きなら、符号を保ってから切る（i8 の 0x81 は i16 で 0xff81） *)
    let n = match v.t with Int (true, 1) when n land 0x80 <> 0 -> n lor (lnot 0xff) | _ -> n in
    { x = int ln (n land m); t }
  | _ when (match v.t with Rec _ -> true | _ -> false) && is_ptr t -> { v with t }   (* SDCC は構造体をポインタへ変換すると番地にする *)
  | _ -> if hl ln (decay v.t) = hl ln t then { v with t } else { x = mk ln (A.Cast (hl ln t, v.x)); t }

let rec lv env (e : expr) : lval =
  let ln = e.ln in
  match e.e with
  | Id x ->
    (match lookup env ln x with
     | Some (V (h, t)) -> LV (h, t)
     | Some (M (l, t)) -> LM (mk ln (A.Addr l), t)
     | Some (K _) -> err ln "定数には代入できない: %s" x
     | None -> err ln "知らない名前: %s" x)
  | Un ("*", p) -> let p' = rv env p in LM (p'.x, elem ln p'.t)
  | Index (a, i) ->
    let a' = rv env a in
    let t = elem ln a'.t in
    let s = size ln t in
    let off =
      match cval env i with
      | Some n -> int ln (n * s land 0xffff)
      | None ->
        (* 256 バイト以下の配列を 8 ビットの添字で引くなら、添字は 8 ビットで計算する（hlang の表と同じ形） *)
        let small = match a'.x.x, a'.t with A.Addr _, _ -> (match ty_lv env a with Arr (_, Some n) -> n * s <= 256 | _ -> false) | _ -> false in
        if small && fits8 env i then mk ln (A.Cast (A.U16, scale ln (rv8 env i) s))
        else scale ln (conv ln (promote_v ln (rv env i)) u16t).x s
    in
    LM (mk ln (A.Bin ("+", a'.x, off)), t)
  | Dot (s, f) ->
    (match lv env s with
     | LM (a, Rec tag) -> let off, t = field ln tag f in LM (add ln a off, t)
     | _ -> err ln ". の左が構造体でない")
  | Arrow (p, f) ->
    let p' = rv env p in
    (match p'.t with
     | Ptr (Rec tag) -> let off, t = field ln tag f in LM (add ln p'.x off, t)
     | _ -> err ln "-> の左が構造体のポインタでない")
  | _ -> err ln "代入できない式"

and load ln = function
  | LV (h, t) -> { x = mk ln (A.Var h); t }
  | LM (a, (Arr (t, _))) -> { x = a; t = Ptr t }
  | LM (a, (Rec _ as t)) -> { x = a; t }
  | LM (a, (Fn _)) -> { x = a; t = Ptr Void }
  | LM (a, t) -> { x = mk ln (A.Mem (hl ln t, a)); t }

and promote_v ln v = if is_int v.t then conv ln v (promote v.t) else v

(* 右辺値。型は C の型（格上げ前） *)
and rv env (e : expr) : v =
  let ln = e.ln in
  match cval env e with
  | Some n when (match e.e with Id _ | Num _ | Chr _ | Bin _ | Un _ | Cast _ | SizeT _ | SizeE _ -> true | _ -> false) ->
    let t = match e.e with Cast (t, _) -> t | _ -> (match ty env e with Int _ as t -> t | _ -> lit_ty n false) in
    { x = int ln (n land (if size ln t = 1 then 0xff else 0xffff)); t }
  | _ ->
  match e.e with
  | Num (n, u) -> { x = int ln (n land 0xffff); t = lit_ty n u }
  | Chr c -> { x = int ln c; t = i16t }
  | Str s -> { x = mk ln (A.Addr (str env ln s)); t = Ptr u8t }
  | Id x when lookup env ln x = None ->
    if Hashtbl.mem env.funs (rn env x) || Hashtbl.mem env.hfuns x then { x = mk ln (A.Addr (rn env x)); t = Ptr Void }
    else err ln "知らない名前: %s" x
  | Un ("*", { e = Post (o, q); _ }) when is_int (elem ln (ty env q)) || is_ptr (elem ln (ty env q)) ->
    (* *p++ は、読んでから進める（hlang の next(p) と同じ形。HL で歩ける） *)
    let p' = rv env q in
    let t = elem ln p'.t in
    let h = tmp env in
    emit env (A.Let (h, Some (hl ln t), mk ln (A.Mem (hl ln t, p'.x))));
    ignore (assign env ln (if o = "++" then "+=" else "-=") q { e = Num (1, false); ln } false);
    { x = mk ln (A.Var h); t }
  | Id _ | Un ("*", _) | Index _ | Dot _ | Arrow _ -> load ln (lv env e)
  | Un ("&", a) ->
    (match a.e with
     | Id f when lookup env ln f = None && (Hashtbl.mem env.funs (rn env f) || Hashtbl.mem env.hfuns f) -> { x = mk ln (A.Addr (rn env f)); t = Ptr Void }
     | _ ->
       match lv env a with
       | LM (x, t) -> { x; t = Ptr t }
       | LV _ -> err ln "番地を取れない（内部の誤り）")
  | Un ("+", a) -> promote_v ln (rv env a)
  | Un ("!", _) -> bool_int env ln (cond env e)
  | Un (o, a) when o = "-" || o = "~" -> let a = promote_v ln (rv env a) in { x = mk ln (A.Un (o, a.x)); t = a.t }
  | Un (o, _) -> err ln "知らない単項演算: %s" o
  | Bin (("==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"), _, _) -> bool_int env ln (cond env e)
  | Bin (o, a, b) -> arith env ln o a b
  | Asg (o, l, r) -> assign env ln o l r true
  | Pre (o, l) -> assign env ln (if o = "++" then "+=" else "-=") l { e = Num (1, false); ln } true
  | Post (o, l) ->
    let old = load ln (lv env l) in
    let t = tmp env in
    emit env (A.Let (t, Some (hl ln old.t), old.x));
    ignore (assign env ln (if o = "++" then "+=" else "-=") l { e = Num (1, false); ln } false);
    { x = mk ln (A.Var t); t = old.t }
  | Call (f, args) ->
    (match call env ln f args with
     | x, Void -> ignore x; err ln "値のない関数を式に使った"
     | x, t -> { x; t })
  | Cast (t, a) -> if is_int t && size ln t = 1 && is_int (ty env a) then { x = (conv ln { x = rv8 env a; t = u8t } t).x; t } else conv ln (rv env a) t
  | SizeT _ | SizeE _ -> assert false                    (* cval で済む *)
  | Cond (c, a, b) ->
    let t = ty env e in
    let h = tmp env in
    emit env (A.Let (h, Some (hl ln t), int ln 0));
    let c = cond env c in
    let pa, xa = collect env (fun () -> to_ty env a t) in
    let pb, xb = collect env (fun () -> to_ty env b t) in
    emit env (A.If (c, pa @ [ A.Set (mk ln (A.Var h), xa) ], pb @ [ A.Set (mk ln (A.Var h), xb) ]));
    { x = mk ln (A.Var h); t }
  | Comma (a, b) -> effect env a; rv env b

(* 条件（真偽）を 0 / 1 の値にする *)
and bool_int env ln c =
  let h = tmp env in
  emit env (A.Let (h, Some A.U8, int ln 0));
  emit env (A.If (c, [ A.Set (mk ln (A.Var h), int ln 1) ], []));
  { x = mk ln (A.Var h); t = u8t }

(* 足し算などの 2 項演算（比較以外） *)
and arith env ln o a b =
  let ta = ty env a and tb = ty env b in
  match o with
  | ("+" | "-") when is_ptr ta && is_int tb ->
    let p = rv env a and i = promote_v ln (rv env b) in
    let s = size ln (elem ln ta) in
    let off = scale ln (conv ln i u16t).x s in
    { x = mk ln (A.Bin (o, p.x, off)); t = decay ta }
  | "+" when is_int ta && is_ptr tb -> arith env ln o b a
  | "-" when is_ptr ta && is_ptr tb ->
    let d = mk ln (A.Bin ("-", (rv env a).x, (rv env b).x)) in
    let s = size ln (elem ln ta) in
    let rec log2 n k = if n = 1 then Some k else if n land 1 = 1 then None else log2 (n lsr 1) (k + 1) in
    (match log2 s 0 with
     | Some 0 -> { x = mk ln (A.Cast (A.I16, d)); t = i16t }
     | Some k -> { x = mk ln (A.Cast (A.I16, mk ln (A.Bin (">>", d, int ln k)))); t = i16t }
     | None -> { x = mk ln (A.Cast (A.I16, mk ln (A.Bin ("/", d, int ln s)))); t = i16t })
  | ">>" when fits8 env a && (match cval env b with Some n -> n >= 0 && n < 8 | None -> false) ->
    (* 0〜255 の値の右シフトは 8 ビットで計算してから広げる（結果は同じ） *)
    conv ln { x = mk ln (A.Bin (">>", rv8 env a, int ln (Option.get (cval env b)))); t = u8t } (promote ta)
  | ("&" | "|" | "^") when is_int ta && is_int tb && (if o = "&" then fits8 env a || fits8 env b else fits8 env a && fits8 env b) ->
    (* 結果が 0〜255 に収まるビット演算も 8 ビットで *)
    conv ln { x = mk ln (A.Bin (o, rv8 env a, rv8 env b)); t = u8t } (usual ta tb)
  | "<<" | ">>" ->
    let a' = promote_v ln (rv env a) in
    (match cval env b with
     | Some n when n >= 0 && n < 16 -> { x = mk ln (A.Bin (o, a'.x, int ln n)); t = a'.t }
     | Some n when n >= 16 && o = "<<" -> { x = int ln 0; t = a'.t }
     | Some _ -> err ln "シフトの量は 0〜15"
     | None -> { x = shift env ln o a'.x (hl ln a'.t) (rv8 env b); t = a'.t })
  | _ ->
    let t = usual ta tb in
    let a' = conv ln (rv env a) t and b' = conv ln (rv env b) t in
    { x = mk ln (A.Bin (o, a'.x, b'.x)); t }

(* 量が変数のシフト。hlang は定数の量しか書けないので、1 つずつずらすループにする *)
and shift env ln o x t n =
  let h = tmp env and c = tmp env in
  let v = mk ln (A.Var h) and k = mk ln (A.Var c) in
  emit env (A.Let (h, Some t, x));
  emit env (A.Let (c, Some A.U8, n));
  emit env (A.While (mk ln (A.Bin ("!=", k, int ln 0)),
                     [ A.Set (v, mk ln (A.Bin (o, v, int ln 1))); A.Set (k, mk ln (A.Bin ("-", k, int ln 1))) ]));
  v

(* (u8)e の値を、8 ビットで計算する式にする。下位 8 ビットが上位に影響されない演算はそのまま 8 ビットで *)
and rv8 env (e : expr) : int A.expr =
  let ln = e.ln in
  let plain () =
    let v = rv env e in
    match v.x.x with
    | A.Int n -> int ln (n land 0xff)
    | _ -> if hl ln (decay v.t) = A.U8 then v.x else mk ln (A.Cast (A.U8, v.x))
  in
  let ints () = match e.e with Bin (_, a, b) -> is_int (ty env a) && is_int (ty env b) | _ -> false in
  match cval env e with
  | Some n -> int ln (n land 0xff)
  | None ->
    match e.e with
    | Bin ((("+" | "-" | "&" | "|" | "^") as o), a, b) when ints () -> mk ln (A.Bin (o, rv8 env a, rv8 env b))
    | Bin ("*", a, b) when ints () -> mk ln (A.Bin ("*", rv8 env a, rv8 env b))
    | Bin ("<<", a, b) when ints () ->
      (match cval env b with
       | Some n when n >= 0 && n < 8 -> mk ln (A.Bin ("<<", rv8 env a, int ln n))
       | Some n when n >= 8 -> int ln 0
       | Some _ -> plain ()
       | None -> shift env ln "<<" (rv8 env a) A.U8 (rv8 env b))
    | Un ((("-" | "~") as o), a) when is_int (ty env a) -> mk ln (A.Un (o, rv8 env a))
    | Cast (Int (_, _), a) when is_int (ty env a) -> rv8 env a
    | _ -> plain ()

(* 式を C の型 t の値にして、hlang の式を返す *)
and to_ty env (e : expr) t : int A.expr =
  let ln = e.ln in
  match t with
  | Int (_, 1) when is_int (ty env e) ->
    let et = ty env e in
    if et = t then (rv env e).x else (conv ln { x = rv8 env e; t = u8t } t).x
  | Rec _ -> err ln "構造体の値は渡せない（ポインタで渡す）"
  | _ ->
    let v = rv env e in
    (conv ln { v with t = decay v.t } t).x

(* 条件。hlang の bool の式を返す *)
and cond env (e : expr) : int A.expr =
  let ln = e.ln in
  match e.e with
  | Bin ((("==" | "!=" | "<" | ">" | "<=" | ">=") as o), a, b) ->
    (match cval env a, cval env b with
     | Some x, Some y -> mk ln (A.Tf (match cval env { e with e = Bin (o, { e = Num (x, false); ln }, { e = Num (y, false); ln }) } with Some 0 -> false | _ -> true))
     | _ ->
       let ta = ty env a and tb = ty env b in
       if is_ptr ta || is_ptr tb then
         mk ln (A.Bin (o, (conv ln (rv env a) u16t).x, (conv ln (rv env b) u16t).x))
       else if fits8 env a && fits8 env b then mk ln (A.Bin (o, rv8 env a, rv8 env b))
       else if fitsi8 env a && fitsi8 env b then mk ln (A.Bin (o, to_ty env a i8t, to_ty env b i8t))
       else
         let t = usual ta tb in
         mk ln (A.Bin (o, (conv ln (rv env a) t).x, (conv ln (rv env b) t).x)))
  | Bin ((("&&" | "||") as o), a, b) ->
    let ca = cond env a in
    let pb, cb = collect env (fun () -> cond env b) in
    if pb = [] then mk ln (A.Bin (o, ca, cb))
    else begin
      (* 右側に前置きの文が要るなら、if で書く（右側は左側しだいで実行されない） *)
      let h = tmp env in
      emit env (A.Let (h, Some A.Bool, mk ln (A.Tf (o = "||"))));
      let set = pb @ [ A.Set (mk ln (A.Var h), cb) ] in
      emit env (if o = "&&" then A.If (ca, set, [ A.Set (mk ln (A.Var h), mk ln (A.Tf false)) ]) else A.If (ca, [], set));
      mk ln (A.Var h)
    end
  | Un ("!", a) -> mk ln (A.Un ("!", cond env a))
  | Comma (a, b) -> effect env a; cond env b
  | _ ->
    (match cval env e with
     | Some n -> mk ln (A.Tf (n <> 0))
     | None ->
       let t = ty env e in
       if is_ptr t then mk ln (A.Bin ("!=", (conv ln (rv env e) u16t).x, int ln 0))
       else if fits8 env e then mk ln (A.Bin ("!=", rv8 env e, int ln 0))
       else let v = rv env e in mk ln (A.Bin ("!=", v.x, int ln 0)))

(* 代入。o は "=" か "+=" など。value なら、代入した後の値を返す *)
and assign env ln o l r value : v =
  match o, l.e with
  | "=", Un ("*", { e = Post (po, q); _ }) when not value && (is_int (elem ln (ty env q)) || is_ptr (elem ln (ty env q))) ->
    (* *p++ = v は、書いてから進める *)
    let t = elem ln (ty env q) in
    let x = to_ty env r t in
    let p' = rv env q in
    emit env (A.Set (mk ln (A.Mem (hl ln t, p'.x)), x));
    ignore (assign env ln (if po = "++" then "+=" else "-=") q { e = Num (1, false); ln } false);
    { x = int ln 0; t = Void }
  | _ ->
  (* 左辺にも副作用があれば、SDCC と同じく右辺を先に計算する（a[f()] = g() は g が先） *)
  let early =
    match ty env l with
    | (Rec _ | Arr _) -> None
    | t when o = "=" && effects l ->
      let h = tmp env in
      emit env (A.Let (h, Some (hl ln t), to_ty env r t));
      Some (mk ln (A.Var h))
    | _ -> None
  in
  let target = lv env l in
  let t = match target with LV (_, t) | LM (_, t) -> t in
  let dst = match target with LV (h, _) -> mk ln (A.Var h) | LM (a, t) -> (match t with Rec _ -> a | _ -> mk ln (A.Mem (hl ln t, a))) in
  (match t with
   | Rec _ ->
     if o <> "=" then err ln "構造体には = だけ";
     (match lv env r with
      | LM (src, t2) when t2 = t ->
        emit env (A.Do (mk ln (A.Call ("copy", [ dst; src; int ln (size ln t) ]))))
      | _ -> err ln "構造体の代入の右辺が同じ型の構造体でない")
   | Arr _ -> err ln "配列には代入できない"
   | _ ->
     let rhs =
       if early <> None then Option.get early
       else if o = "=" then to_ty env r t
       else begin
         (match target with LM _ when effects l -> err ln "副作用のある左辺への複合代入は未対応" | _ -> ());
         let op = String.sub o 0 (String.length o - 1) in
         to_ty env { e = Bin (op, l, r); ln } t
       end
     in
     emit env (A.Set (dst, rhs)));
  if value then load ln target else { x = int ln 0; t = Void }

(* 関数の呼び出し。戻り値は (hlang の式, C の戻りの型) *)
and call env ln f args =
  let name = match f.e with Id n -> n | _ -> err ln "関数ポインタでの呼び出しは未対応" in
  let ps, ret, casts =
    match Hashtbl.find_opt env.funs (rn env name), Hashtbl.find_opt env.hfuns name with
    | Some (r, ps), _ -> ps, r, List.map (fun _ -> None) ps
    | None, Some (ps, r) ->
      List.map of_hl ps, of_hl r,
      List.map (function (A.Ptr _ | A.Enum _) as t -> Some t | _ -> None) ps
    | None, None -> err ln "知らない関数: %s" name
  in
  if List.length ps <> List.length args then err ln "%s の引数は %d 個" name (List.length ps);
  let xs =
    List.map2
      (fun (t, c) a ->
        let x = to_ty env a t in
        match c with Some h -> mk ln (A.Cast (h, x)) | None -> x)
      (List.combine ps casts) args
  in
  let x = mk ln (A.Call (rn env name, xs)) in
  match ret with
  | Void -> x, Void
  | _ ->
    (* 呼び出しは、ほかの前置きの文と順番が入れ替わらないよう、一時変数へ受ける *)
    let h = tmp env in
    emit env (A.Let (h, Some (hl ln ret), x));
    mk ln (A.Var h), ret

(* 値を使わない式（文としての式） *)
and effect env (e : expr) =
  let ln = e.ln in
  match e.e with
  | Asg (o, l, r) -> ignore (assign env ln o l r false)
  | Pre (o, l) | Post (o, l) -> ignore (assign env ln (if o = "++" then "+=" else "-=") l { e = Num (1, false); ln } false)
  | Call (f, args) -> let x, _ = call_stmt env ln f args in emit env (A.Do x)
  | Comma (a, b) -> effect env a; effect env b
  | Cond (c, a, b) ->
    let c = cond env c in
    let pa, () = collect env (fun () -> effect env a) in
    let pb, () = collect env (fun () -> effect env b) in
    emit env (A.If (c, pa, pb))
  | Cast (_, a) -> effect env a
  | _ -> if effects e then ignore (rv env e)

and call_stmt env ln f args =
  (* 戻り値を使わない呼び出し: 一時変数に受けずに、そのまま Do にする *)
  let name = match f.e with Id n -> n | _ -> err ln "関数ポインタでの呼び出しは未対応" in
  match Hashtbl.find_opt env.funs (rn env name), Hashtbl.find_opt env.hfuns name with
  | None, None -> err ln "知らない関数: %s" name
  | _ ->
    let saved = env.pre in
    env.pre <- [];
    let x, t = call env ln f args in
    (match x.x, env.pre with
     | A.Var h, A.Let (h', _, c) :: rest when h = h' -> env.pre <- rest @ saved; c, t
     | _ -> env.pre <- env.pre @ saved; x, t)

(* ---- 初期化子を、型に沿ったバイトの並びにする ---- *)

type item = B of int | W of int A.expr        (* 定数のバイト、または起動時に書く 16 ビットの番地 *)

let rec fill env ln t (q : init list ref) : item list =
  let take () = match !q with x :: r -> q := r; Some x | [] -> None in
  let scalar e =
    match cval env e with
    | Some v -> List.init (size ln t) (fun k -> B ((v lsr (8 * k)) land 0xff))
    | None ->
      if size ln t <> 2 then err ln "定数でない初期化子";
      [ W (to_ty env e t) ]
  in
  match t with
  | Int _ | Ptr _ ->
    (match take () with
     | Some (I e) -> scalar e
     | Some (L [ I e ]) -> scalar e
     | Some (L _) -> err ln "スカラーの初期化子に { } が多い"
     | None -> List.init (size ln t) (fun _ -> B 0))
  | Arr (et, n) ->
    (match !q, et with
     | I { e = Str s; _ } :: r, Int (_, 1) ->
       (* char の配列を文字列で初期化 *)
       q := r;
       let bs = List.map (fun c -> B (Char.code c)) (List.of_seq (String.to_seq s)) @ [ B 0 ] in
       let n = Option.value n ~default:(List.length bs) in
       List.filteri (fun i _ -> i < n) bs @ List.init (max 0 (n - List.length bs)) (fun _ -> B 0)
     | L items :: r, _ ->
       q := r;
       let inner = ref items in
       let rec go k acc =
         if (match n with Some n -> k >= n | None -> !inner = []) then List.concat (List.rev acc)
         else go (k + 1) (fill env ln et inner :: acc)
       in
       go 0 []
     | _, _ ->
       (* 中括弧の省略: 外側の並びから順に取る *)
       let n = match n with Some n -> n | None -> err ln "大きさの分からない配列" in
       List.concat (List.init n (fun _ -> fill env ln et q)))
  | Rec tag ->
    let r = Hashtbl.find structs tag in
    let mems = if r.union then (match r.mems with m :: _ -> [ m ] | [] -> []) else r.mems in
    let pad items = items @ List.init (size ln t - List.length items) (fun _ -> B 0) in
    let members q = List.concat_map (fun (_, mt) -> fill env ln mt q) mems in
    (match !q with
     | L items :: rest -> q := rest; pad (members (ref items))
     | _ -> pad (members q))
  | Void | Fn _ -> err ln "初期化できない型"

let items env ln t init = fill env ln t (ref [ init ])

(* 配列の大きさを初期化子から決める *)
let complete env ln t init =
  match t, init with
  | Arr (et, None), Some (L xs) ->
    (* 要素が構造体や配列なら、要素ごとに { } で囲んである形だけ *)
    (match et with
     | (Rec _ | Arr _) when List.exists (function I _ -> true | L _ -> false) xs -> err ln "大きさを書いてください（{ } を省いた初期化子）"
     | _ -> Arr (et, Some (List.length xs)))
  | Arr (Int (_, 1), None), Some (I { e = Str s; _ }) -> Arr (Int (false, 1), Some (String.length s + 1))
  | Arr (_, None), _ -> err ln "大きさの分からない配列"
  | _ -> ignore env; t

(* 番地 a から items を置く文（起動時、または宣言の所で） *)
let store_items ln a items =
  let rec go off = function
    | [] -> []
    | B v :: rest -> A.Set (mk ln (A.Mem (A.U8, add ln a off)), int ln v) :: go (off + 1) rest
    | W x :: rest -> A.Set (mk ln (A.Mem (A.U16, add ln a off)), x) :: go (off + 2) rest
  in
  go 0 items

let all_const items = List.for_all (function B _ -> true | W _ -> false) items
let bytes items = List.map (function B v -> v | W _ -> 0) items

(* 番地 a に items を書く。定数だけで長ければ、ROM の表から copy する *)
let init_at env ln name a items =
  if all_const items && List.length items > 8 then begin
    let d = name ^ ".init" in
    env.tops <- A.Data (d, A.U8, List.map (int ln) (bytes items), ln) :: env.tops;
    [ A.Do (mk ln (A.Call ("copy", [ a; mk ln (A.Addr d); int ln (List.length items) ]))) ]
  end
  else store_items ln a items

(* ---- 文 ---- *)

(* 文の中で、変数 x に代入しているか、番地を取っているか *)
let rec touches x (s : stmt) =
  let rec ex (e : expr) =
    match e.e with
    | Asg (_, { e = Id y; _ }, r) -> y = x || ex r
    | Pre (_, { e = Id y; _ }) | Post (_, { e = Id y; _ }) | Un ("&", { e = Id y; _ }) -> y = x
    | Num _ | Chr _ | Str _ | Id _ | SizeT _ | SizeE _ -> false
    | Bin (_, a, b) | Index (a, b) | Comma (a, b) | Asg (_, a, b) -> ex a || ex b
    | Un (_, a) | Dot (a, _) | Arrow (a, _) | Cast (_, a) | Pre (_, a) | Post (_, a) -> ex a
    | Cond (a, b, c) -> ex a || ex b || ex c
    | Call (f, args) -> ex f || List.exists ex args
  in
  let ex_opt = function Some e -> ex e | None -> false in
  match s.s with
  | Expr e | Case e -> ex e
  | Decl ds -> List.exists (fun d -> d.name = x || (match d.init with Some (I e) -> ex e | _ -> false)) ds
  | Block ss -> List.exists (touches x) ss
  | If (c, a, b) -> ex c || touches x a || (match b with Some b -> touches x b | None -> false)
  | While (c, b) | Do (b, c) | Switch (c, b) -> ex c || touches x b
  | For (i, c, st, b) -> (match i with Some i -> touches x i | None -> false) || ex_opt c || ex_opt st || touches x b
  | Return e -> ex_opt e
  | Default | Break | Continue | AsmS _ | Empty -> false

let rec stmt env (s : stmt) : int A.stmt list =
  let ln = s.sln in
  let pre f = let p, r = collect env f in p @ r in
  match s.s with
  | Empty -> []
  | Expr e -> pre (fun () -> effect env e; [])
  | Decl ds -> List.concat_map (local env) ds
  | Block ss -> block env ss
  | If (c, a, b) ->
    pre (fun () ->
        let c = cond env c in
        [ A.If (c, stmt env a, match b with Some b -> stmt env b | None -> []) ])
  | While (c, b) ->
    let p, c = collect env (fun () -> cond env c) in
    let body = loop env [] b in
    if p = [] then [ A.While (c, body) ]
    else [ A.While (mk ln (A.Tf true), p @ [ A.If (mk ln (A.Un ("!", c)), [ A.Break ], []) ] @ body) ]
  | Do (b, c) ->
    (* loop { 本体; if !c break }。continue は、条件を調べてから戻る *)
    let p, c = collect env (fun () -> cond env c) in
    let check = p @ [ A.If (mk ln (A.Un ("!", c)), [ A.Break ], []) ] in
    [ A.While (mk ln (A.Tf true), loop env check b @ check) ]
  | For (init, c, step, b) -> for_ env ln init c step b
  | Switch (e, b) -> switch env ln e b
  | Case _ | Default -> err ln "case が switch の外にある"
  | Break ->
    (match env.brk with
     | `Loop :: _ -> [ A.Break ]
     | `Switch :: _ -> err ln "switch の途中の break は未対応（枝の最後に置く）"
     | [] -> err ln "break がループの外にある")
  | Continue ->
    (match env.conts with
     | c :: _ -> c @ [ A.Continue ]
     | [] -> err ln "continue がループの外にある")
  | Return None -> [ A.Return None ]
  | Return (Some e) ->
    if env.ret = Void then pre (fun () -> effect env e; [ A.Return None ])
    else pre (fun () -> let x = to_ty env e env.ret in [ A.Return (Some x) ])
  | AsmS ts ->
    (* 関数の途中のアセンブラ: 引数のない関数に切り出して呼ぶ。全部のレジスタを壊すとみなされ、
       生きている値は呼び出しと同じく push / pop で守られる *)
    env.n <- env.n + 1;
    let name = Printf.sprintf "%s.asm%d" env.fname env.n in
    env.tops <- A.AsmFn (name, [], A.Unit, asm_lines env name ts @ [ Asm.Ins ("ret", []) ], ln) :: env.tops;
    [ A.Do (mk ln (A.Call (name, []))) ]

(* asm の字句を組み立ての行にする。_name は C の大域の名前（関数か、メモリに置いた変数） *)
and asm_lines env name ts =
  let sym ln x =
    match List.assoc_opt x env.scope, lookup env ln x with
    | Some (V _), _ -> err ln "アセンブラから局所変数 %s は参照できない" x
    | _, Some (M (l, _)) | _, Some (V (l, _)) -> l
    | _, Some (K _) -> err ln "アセンブラから定数 %s は参照できない（内部の誤り）" x
    | _, None -> rn env x                            (* 関数か、アセンブラの中のラベル *)
  in
  Casm.lines ~name ~sym ts

and block env ss =
  let saved = env.scope in
  let r = List.concat_map (stmt env) ss in
  env.scope <- saved;
  r

(* ループの本体。cont は continue の前に出す文 *)
and loop env cont b =
  env.conts <- cont :: env.conts;
  env.brk <- `Loop :: env.brk;
  let r = stmt env b in
  env.conts <- List.tl env.conts;
  env.brk <- List.tl env.brk;
  r

and for_ env ln init c step b =
  let saved = env.scope in
  (* for (u8 i = a; i < N; i++) で、本体が i を変えないなら、hlang の for i in a..N にする *)
  let simple =
    match init, c, step with
    | Some { s = Decl [ { name; ty = (Int (false, _) as t); init = Some (I lo); st = Auto; _ } ]; _ },
      Some { e = Bin ("<", { e = Id x; _ }, hi); _ },
      Some { e = (Post ("++", { e = Id y; _ }) | Pre ("++", { e = Id y; _ }) | Asg ("+=", { e = Id y; _ }, { e = Num (1, _); _ })); _ }
      when x = name && y = name && not (touches name b) && not (Hashtbl.mem env.taken name) ->
      (match cval env hi with
       | Some n when n >= 0 && n <= (if t = u8t then 255 else 65535) -> Some (name, t, lo, n)
       | _ -> None)
    | _ -> None
  in
  let r =
    match simple with
    | Some (name, t, lo, n) ->
      let p, lo = collect env (fun () -> to_ty env lo t) in
      let h = fresh env name in
      env.scope <- (name, V (h, t)) :: env.scope;
      let body = loop env [] b in
      p @ [ A.For (h, lo, int ln n, body) ]
    | None ->
      let first = match init with Some s -> stmt env s | None -> [] in
      let st = match step with Some e -> let p, () = collect env (fun () -> effect env e) in p | None -> [] in
      let p, c = match c with Some c -> collect env (fun () -> cond env c) | None -> [], mk ln (A.Tf true) in
      let body = loop env st b in
      if p = [] then first @ [ A.While (c, body @ st) ]
      else first @ [ A.While (mk ln (A.Tf true), p @ [ A.If (mk ln (A.Un ("!", c)), [ A.Break ], []) ] @ body @ st) ]
  in
  env.scope <- saved;
  r

(* switch: 枝ごとに、最後が break / return / continue で終わるものだけ。8 ビットの値なら hlang の match *)
and switch env ln e b =
  let ss = match b.s with Block ss -> ss | _ -> [ b ] in
  (* (ラベルの並び, default か, 文) に分ける *)
  (* body は逆順。枝の最後（先頭）が break / return / continue でなければ、次の枝へ落ち抜ける *)
  let groups = ref [] and labels = ref [] and dflt = ref false and body = ref [] in
  let ends = function { s = Break | Return _ | Continue; _ } :: _ -> true | _ -> false in
  let close () =
    if !body <> [] || !labels <> [] || !dflt then groups := (List.rev !labels, !dflt, List.rev !body) :: !groups;
    labels := []; dflt := false; body := []
  in
  List.iter
    (fun (s : stmt) ->
      match s.s with
      | Case _ | Default ->
        if !body <> [] then (if not (ends !body) then err s.sln "case の落ち抜け（break のない枝）は未対応"; close ());
        (match s.s with
         | Case c -> (match cval env c with Some v -> labels := v :: !labels | None -> err s.sln "case の値は定数")
         | _ -> dflt := true)
      | _ -> body := s :: !body)
    ss;
  close ();
  let groups = List.rev !groups in
  let strip ss = match List.rev ss with { s = Break; _ } :: r -> List.rev r | _ -> ss in
  env.brk <- `Switch :: env.brk;
  let lower ss = block env (strip ss) in
  let p, r =
    collect env (fun () ->
        let all = List.concat_map (fun (ls, _, _) -> ls) groups in
        if fits8 env e && List.for_all (fun v -> v >= 0 && v <= 255) all then begin
          let x = rv8 env e in
          let arms = List.filter_map (fun (ls, d, ss) -> if d then None else Some (List.map (int ln) ls, lower ss)) groups in
          let other = List.find_map (fun (_, d, ss) -> if d then Some (lower ss) else None) groups in
          (* default にもラベルが付いていれば、その値の枝も同じ本体にする *)
          let arms = arms @ List.filter_map (fun (ls, d, ss) -> if d && ls <> [] then Some (List.map (int ln) ls, lower ss) else None) groups in
          [ A.Match (x, arms, other) ]
        end else begin
          let v = rv env e in
          let t = promote v.t in
          let h = tmp env in
          emit env (A.Let (h, Some (hl ln t), (conv ln v t).x));
          let test ls = List.fold_left (fun acc l ->
              let c = mk ln (A.Bin ("==", mk ln (A.Var h), int ln (l land 0xffff))) in
              match acc with None -> Some c | Some a -> Some (mk ln (A.Bin ("||", a, c)))) None ls in
          let dflt = match List.find_opt (fun (_, d, _) -> d) groups with Some (_, _, ss) -> lower ss | None -> [] in
          let chain = List.fold_right (fun (ls, d, ss) rest ->
              match test ls with
              | Some c when not d -> [ A.If (c, lower ss, rest) ]
              | _ -> rest) groups dflt in
          chain
        end)
  in
  env.brk <- List.tl env.brk;
  p @ r

(* 局所変数の宣言 *)
and local env (d : decl) =
  let ln = d.dln in
  let t = complete env ln d.ty d.init in
  match d.st, t with
  | (Typedef | Extern), _ | _, Fn _ -> []
  | _ when d.st = Static || not (is_int t || is_ptr t) || Hashtbl.mem env.taken d.name ->
    (* 固定の番地に置く。static なら初期化は起動時に 1 回、そうでなければ宣言の所で *)
    let l = fresh env (env.fname ^ "." ^ d.name) in
    env.tops <- A.Array (l, A.U8, int ln (size ln t), ln) :: env.tops;
    env.scope <- (d.name, M (l, t)) :: env.scope;
    let a = mk ln (A.Addr l) in
    (match d.init with
     | None -> []
     | Some i ->
       let p, its = collect env (fun () -> items env ln t i) in
       let code = p @ init_at env ln l a its in
       if d.st = Static then (env.inits <- List.rev_append code env.inits; []) else code)
  | _ ->
    let h = fresh env d.name in
    let p, x =
      collect env (fun () ->
          match d.init with
          | Some (I e) -> to_ty env e t
          | Some (L [ I e ]) -> to_ty env e t
          | Some (L _) -> err ln "スカラーの初期化子に { } が多い"
          | None -> int ln 0)
    in
    env.scope <- (d.name, V (h, t)) :: env.scope;
    p @ [ A.Let (h, Some (hl ln t), x) ]

(* ---- ファイル全体 ---- *)

(* 本体がアセンブラと、引数を書いただけの文（hl; など。SDCC で使っていない警告を消すためのもの）だけか *)
let asm_only body =
  List.exists (fun (s : stmt) -> match s.s with AsmS _ -> true | _ -> false) body
  && List.for_all (fun (s : stmt) -> match s.s with AsmS _ | Empty | Expr { e = Id _; _ } -> true | _ -> false) body

(* SDCC の呼び出し規約（sdcccall(1)）の引数のレジスタ。1 つ目は A か HL。2 つ目は、1 つ目が A なら L か HL、
   1 つ目が HL なら E か DE。3 つ目からはスタックで、ここでは受けない *)
let sdcc_regs ln name ts =
  let one t = if size ln t = 1 then "a" else "hl" in
  match ts with
  | [] -> []
  | [ a ] -> [ one a ]
  | [ a; b ] ->
    let r1 = one a in
    [ r1; (match r1, size ln b with "a", 1 -> "l" | "a", _ -> "hl" | _, 1 -> "e" | _ -> "de") ]
  | _ -> err ln "%s: 引数が 3 つ以上のアセンブラの関数は未対応（SDCC はスタックで渡す）" name

(* & を取られる名前を集める（そういう変数はメモリに置く） *)
let scan_taken env tops =
  let rec ex (e : expr) =
    match e.e with
    | Un ("&", { e = Id x; _ }) -> Hashtbl.replace env.taken x ()
    | Num _ | Chr _ | Str _ | Id _ | SizeT _ | SizeE _ -> ()
    | Bin (_, a, b) | Index (a, b) | Comma (a, b) | Asg (_, a, b) -> ex a; ex b
    | Un (_, a) | Dot (a, _) | Arrow (a, _) | Cast (_, a) | Pre (_, a) | Post (_, a) -> ex a
    | Cond (a, b, c) -> ex a; ex b; ex c
    | Call (f, args) -> ex f; List.iter ex args
  in
  let rec ini = function I e -> ex e | L xs -> List.iter ini xs in
  let rec st (s : stmt) =
    match s.s with
    | Expr e | Case e -> ex e
    | Decl ds -> List.iter (fun d -> Option.iter ini d.init) ds
    | Block ss -> List.iter st ss
    | If (c, a, b) -> ex c; st a; Option.iter st b
    | While (c, b) | Do (b, c) | Switch (c, b) -> ex c; st b
    | For (i, c, s, b) -> Option.iter st i; Option.iter ex c; Option.iter ex s; st b
    | Return e -> Option.iter ex e
    | AsmS ts ->
      (* アセンブラが _name で参照する C の変数は、メモリに置く *)
      List.iter (fun (t : Cpp.tok) -> match t.k with Cpp.Id x when String.length x > 1 && x.[0] = '_' ->
          Hashtbl.replace env.taken (String.sub x 1 (String.length x - 1)) () | _ -> ()) ts
    | _ -> ()
  in
  List.iter
    (function
      | TFun f -> List.iter st f.body
      | TDecl d -> Option.iter ini d.init
      | _ -> ())
    tops

(* static の名前は .c ファイルごとに別物。ほかのファイルの static や大域とぶつかるものは「名前.ファイルの番号」にする。
   宣言の名前を先に付け直し、ファイルの中から C の名前で引けるよう ren に入れる *)
let rename_statics env (tops : (int * top) list) =
  let static_name = function
    | TDecl d when d.st = Static -> Some d.name
    | TFun f when f.st = Static -> Some f.name
    | _ -> None
  in
  let public_name = function
    | TDecl d when d.st <> Static && d.st <> Typedef -> Some d.name
    | TFun f when f.st <> Static -> Some f.name
    | _ -> None
  in
  let statics = List.sort_uniq compare (List.filter_map (fun (k, t) -> Option.map (fun n -> n, k) (static_name t)) tops) in
  let publics = List.filter_map (fun (_, t) -> public_name t) tops in
  let clash n k = List.mem n publics || List.exists (fun (m, j) -> m = n && j <> k) statics in
  List.iter (fun (n, k) ->
      if clash n k then begin
        Hashtbl.replace env.ren (k, n) (Printf.sprintf "%s.%d" n k);
        if Hashtbl.mem env.taken n then Hashtbl.replace env.taken (Printf.sprintf "%s.%d" n k) ()
      end) statics;
  let re k n = Option.value (Hashtbl.find_opt env.ren (k, n)) ~default:n in
  List.map
    (fun (k, t) ->
      k,
      match t with
      | TDecl d when d.st = Static -> TDecl { d with name = re k d.name }
      | TFun f when f.st = Static -> TFun { f with name = re k f.name }
      | t -> t)
    tops

(* 本体が C の __naked 関数には、SDCC は ret を出さない。最後まで来ると、同じファイルの次の関数へ落ちて続きを実行する
   （damaxian の AppInitialize は、そのまま AppLoad を実行する）。C の版と同じに動くよう、最後に次の関数を呼ぶ *)
let fall_through (tops : (int * top) list) =
  let rec go = function
    | (k, TFun f) :: rest when f.naked && not (asm_only f.body) ->
      let next = List.find_map (function j, TFun g when j = k -> Some (g.name, g.ps = []) | _ -> None) rest in
      let body =
        match next with
        | Some (g, true) -> f.body @ [ { s = Expr { e = Call ({ e = Id g; ln = f.fln }, []); ln = f.fln }; sln = f.fln } ]
        | Some (g, false) -> err f.fln "%s: __naked の C の関数は、次の関数 %s へ落ちるが、%s には引数がある" f.name g g
        | None -> f.body
      in
      (k, TFun { f with body }) :: go rest
    | t :: rest -> t :: go rest
    | [] -> []
  in
  go tops

let program ~(hl_sigs : string -> (string * hsig) list) (tus : top list list) : int A.top list =
  let env = {
    globals = Hashtbl.create 64; funs = Hashtbl.create 64; hfuns = Hashtbl.create 64; enums = Hashtbl.create 64;
    taken = Hashtbl.create 16; strs = Hashtbl.create 16; tops = []; inits = []; scope = []; used = []; pre = [];
    fname = ""; ret = Void; n = 0; conts = []; brk = []; tu = 0; ren = Hashtbl.create 8;
  } in
  let tops = List.concat (List.mapi (fun k ts -> List.map (fun t -> k, t) ts) tus) in
  scan_taken env (List.map snd tops);
  let tops = rename_statics env tops in
  let tops = fall_through tops in
  (* hlang の組み込み: 割り込みの許可と禁止、copy など *)
  List.iter (fun f -> Hashtbl.replace env.hfuns f ([], A.Unit)) Asm.bare;
  (* 固定小数点の掛け算: fmul(a, b, n) = (a * b) >> n（32 ビットの積）。fmulu は符号なし *)
  Hashtbl.replace env.hfuns "fmul" ([ A.I16; A.I16; A.U8 ], A.I16);
  Hashtbl.replace env.hfuns "fmulu" ([ A.U16; A.U16; A.U8 ], A.U16);
  List.iter (fun (f, (regs, _, _)) ->
      if f.[0] <> '.' then
        Hashtbl.replace env.hfuns f (List.map (fun r -> if String.length r = 2 then A.U16 else A.U8) regs, if f = "inp" then A.U8 else A.Unit))
    Asm.prims;
  (* 1 回目: 列挙の定数、関数の型、大域の置き場所 *)
  let imports = ref [] in
  List.iter
    (fun (k, t) ->
      env.tu <- k;
      match t with
      | TEnum cs ->
        ignore (List.fold_left (fun next (n, v, ln) ->
            let v = match v with Some e -> (match cval env e with Some v -> v | None -> err ln "列挙の値は定数") | None -> next in
            Hashtbl.replace env.enums n v;
            v + 1) 0 cs)
      | TImport (f, ln) ->
        imports := A.Import (f, ln) :: !imports;
        List.iter (fun (n, s) -> Hashtbl.replace env.hfuns n s) (hl_sigs f)
      | TFun f ->
        if Hashtbl.mem env.hfuns f.name then err f.fln "hlang の側（#include した .hl か組み込み）にも %s がある" f.name;
        Hashtbl.replace env.funs f.name (f.ret, List.map snd f.ps)
      | TDecl { name; ty = Fn (r, ps); st; _ } when st <> Typedef -> Hashtbl.replace env.funs name (r, ps)
      | TDecl _ -> ())
    tops;
  (* 大域変数。extern だけで定義のないものは、定義の方を待つ *)
  let defs = List.filter_map (function k, TDecl d when d.st <> Extern && d.st <> Typedef && (match d.ty with Fn _ -> false | _ -> true) -> Some (k, d) | _ -> None) tops in
  let externs = List.filter_map (function _, TDecl d when d.st = Extern && (match d.ty with Fn _ -> false | _ -> true) -> Some d | _ -> None) tops in
  (* 同じ名前が何度も（extern の後に定義など）出てきたら、初期化子のある方 *)
  let defs =
    List.fold_left (fun acc ((_, (d : decl)) as kd) ->
        match List.find_opt (fun (_, (x : decl)) -> x.name = d.name) acc with
        | Some (_, x) when x.init <> None -> acc
        | Some _ -> kd :: List.filter (fun (_, (x : decl)) -> x.name <> d.name) acc
        | None -> kd :: acc) [] defs |> List.rev
  in
  List.iter
    (fun (k, (d : decl)) ->
      env.tu <- k;
      let ln = d.dln in
      let t = complete env ln d.ty d.init in
      let scalar = (is_int t || (match t with Ptr _ -> true | _ -> false)) && not (Hashtbl.mem env.taken d.name) in
      match d.const, d.init with
      | true, Some init when scalar ->
        (match init with
         | I e when cval env e <> None -> Hashtbl.replace env.globals d.name (K (Option.get (cval env e), t))
         | _ ->
           env.tops <- A.Global (d.name, hl ln t, None) :: env.tops;
           Hashtbl.replace env.globals d.name (V (d.name, t));
           let p, x = collect env (fun () -> match init with I e | L [ I e ] -> to_ty env e t | _ -> err ln "スカラーの初期化子に { } が多い") in
           env.inits <- List.rev_append (p @ [ A.Set (mk ln (A.Var d.name), x) ]) env.inits)
      | true, Some init ->
        (* 定数の表は ROM へ。番地を含むなら（ポインタの表）、RAM に置いて起動時に書く *)
        Hashtbl.replace env.globals d.name (M (d.name, t));
        let p, its = collect env (fun () -> items env ln t init) in
        if all_const its && p = [] then env.tops <- A.Data (d.name, A.U8, List.map (int ln) (bytes its), ln) :: env.tops
        else begin
          env.tops <- A.Array (d.name, A.U8, int ln (size ln t), ln) :: env.tops;
          env.inits <- List.rev_append (p @ init_at env ln d.name (mk ln (A.Addr d.name)) its) env.inits
        end
      | _, init when scalar ->
        Hashtbl.replace env.globals d.name (V (d.name, t));
        (match init with
         | Some (I e | L [ I e ]) when cval env e <> None ->
           let v = Option.get (cval env e) in
           let m = if size ln t = 1 then 0xff else 0xffff in
           env.tops <- A.Global (d.name, hl ln t, Some (int ln (v land m))) :: env.tops
         | None -> env.tops <- A.Global (d.name, hl ln t, None) :: env.tops
         | Some init ->
           env.tops <- A.Global (d.name, hl ln t, None) :: env.tops;
           let p, x = collect env (fun () -> match init with I e | L [ I e ] -> to_ty env e t | _ -> err ln "スカラーの初期化子に { } が多い") in
           env.inits <- List.rev_append (p @ [ A.Set (mk ln (A.Var d.name), x) ]) env.inits)
      | _, init ->
        Hashtbl.replace env.globals d.name (M (d.name, t));
        env.tops <- A.Array (d.name, A.U8, int ln (size ln t), ln) :: env.tops;
        (match init with
         | None -> ()
         | Some i ->
           let p, its = collect env (fun () -> items env ln t i) in
           env.inits <- List.rev_append (p @ init_at env ln d.name (mk ln (A.Addr d.name)) its) env.inits))
    defs;
  List.iter (fun (d : decl) -> if not (Hashtbl.mem env.globals d.name) then err d.dln "extern の %s に定義がない" d.name) externs;
  (* 2 回目: 関数 *)
  let fns =
    List.filter_map
      (fun (k, t) ->
        env.tu <- k;
        match t with
        | TFun f when f.naked && asm_only f.body ->
          (* 本体がアセンブラだけの __naked 関数。引数は SDCC の呼び出し規約（sdcccall(1)）のレジスタで受ける *)
          env.fname <- f.name;
          env.scope <- [];
          let regs = sdcc_regs f.fln f.name (List.map snd f.ps) in
          let ret = match f.ret with
            | Void -> A.Unit
            | t when size f.fln t = 1 -> hl f.fln t
            | _ -> err f.fln "%s: 16 ビットを返すアセンブラの関数は未対応（SDCC は DE で返す）" f.name
          in
          let lines = List.concat_map (fun (s : stmt) -> match s.s with AsmS ts -> asm_lines env f.name ts | _ -> []) f.body in
          env.tops <- A.AsmFn (f.name, List.map2 (fun (_, t) r -> hl f.fln t, r) f.ps regs, ret, lines, f.fln) :: env.tops;
          None
        | TFun f ->
          env.fname <- f.name;
          env.used <- [];
          env.scope <- [];
          env.pre <- [];
          env.n <- 0;
          env.ret <- (if f.name = "main" then Void else f.ret);
          let ps, entry =
            List.split
              (List.map
                 (fun (x, t) ->
                   if x = "" then err f.fln "%s: 名前のない引数" f.name;
                   (match t with Rec _ -> err f.fln "構造体の値は引数にできない（ポインタで渡す）" | _ -> ());
                   let h = fresh env x in
                   if Hashtbl.mem env.taken x then begin
                     (* 番地を取られる引数は、入口でメモリへ写す *)
                     let l = fresh env (f.name ^ "." ^ x) in
                     env.tops <- A.Array (l, A.U8, int f.fln (size f.fln t), f.fln) :: env.tops;
                     env.scope <- (x, M (l, t)) :: env.scope;
                     (h, hl f.fln t), [ A.Set (mk f.fln (A.Mem (hl f.fln t, mk f.fln (A.Addr l))), mk f.fln (A.Var h)) ]
                   end else begin
                     env.scope <- (x, V (h, t)) :: env.scope;
                     (h, hl f.fln t), []
                   end)
                 f.ps)
          in
          let body = List.concat entry @ block env f.body in
          Some (f.name, f.inline, f.ret, f.fln, ps, body)
        | _ -> None)
      tops
  in
  let fns =
    List.map
      (fun (name, inline, ret, fln, ps, body) ->
        let body = if name = "main" then List.rev env.inits @ body else body in
        A.Fn ({ A.name; ps; ret = (if name = "main" then A.Unit else hl fln ret); body; lut = false; inline;
                clobbers = None; task = false; pool = 0; self = "" }, fln))
      fns
  in
  List.rev !imports @ List.rev env.tops @ fns

(* ---- 割り込みと main の両方から呼ぶ関数を、割り込み用に複製する ----
   hlang は局所変数を固定の番地に置くので、同じ関数を割り込みからも main からも呼ぶと、変数が壊れる（8 章）。
   C のプログラムはそれを気にしないので、両方からたどれる関数は「名前.isr」という複製を作り、
   割り込みの側（&関数 で番地を取られる関数からたどれる所）の呼び出しは複製へ付け替える。 *)

let rec map_e f (e : int A.expr) : int A.expr =
  let m = map_e f in
  let x =
    match e.x with
    | A.Call (g, args) -> A.Call (f g, List.map m args)
    | A.Addr g -> A.Addr (f g)
    | A.Index (a, b) -> A.Index (m a, m b)
    | A.Field (a, n) -> A.Field (m a, n)
    | A.Lut (g, a) -> A.Lut (g, m a)
    | A.Bin (o, a, b) -> A.Bin (o, m a, m b)
    | A.Un (o, a) -> A.Un (o, m a)
    | A.Mem (t, a) -> A.Mem (t, m a)
    | A.Port a -> A.Port (m a)
    | A.Cast (t, a) -> A.Cast (t, m a)
    | A.Spawn (g, args) -> A.Spawn (g, List.map m args)
    | (A.Int _ | A.Tf _ | A.Str _ | A.Var _ | A.Glob _ | A.Running _) as x -> x
  in
  { e with x }

let rec map_s f (s : int A.stmt) : int A.stmt =
  let e = map_e f and b = List.map (map_s f) in
  match s with
  | A.Let (x, t, v) -> A.Let (x, t, e v)
  | A.Set (l, v) -> A.Set (e l, e v)
  | A.If (c, t, k) -> A.If (e c, b t, b k)
  | A.While (c, body) -> A.While (e c, b body)
  | A.For (x, lo, hi, body) -> A.For (x, e lo, e hi, b body)
  | A.Match (v, arms, other) -> A.Match (e v, List.map (fun (ps, body) -> ps, b body) arms, Option.map b other)
  | A.Return v -> A.Return (Option.map e v)
  | A.Do v -> A.Do (e v)
  | A.Wait v -> A.Wait (e v)
  | A.Each (x, t, body) -> A.Each (x, t, b body)
  | A.Stop (t, v) -> A.Stop (t, Option.map e v)
  | (A.Break | A.Continue | A.Yield | A.Resume _) as s -> s

(* 本体が呼ぶ関数と、番地を取る名前 *)
let refs body =
  let calls = ref [] and addrs = ref [] in
  let rec ex (e : int A.expr) =
    (match e.x with A.Call (g, _) -> calls := g :: !calls | A.Addr g -> addrs := g :: !addrs | _ -> ());
    match e.x with
    | A.Call (_, args) | A.Spawn (_, args) -> List.iter ex args
    | A.Index (a, b) | A.Bin (_, a, b) -> ex a; ex b
    | A.Field (a, _) | A.Lut (_, a) | A.Un (_, a) | A.Mem (_, a) | A.Port a | A.Cast (_, a) -> ex a
    | _ -> ()
  in
  let rec st = function
    | A.Let (_, _, v) | A.Do v | A.Wait v | A.Return (Some v) | A.Stop (_, Some v) -> ex v
    | A.Set (l, v) -> ex l; ex v
    | A.If (c, t, k) -> ex c; List.iter st t; List.iter st k
    | A.While (c, b) -> ex c; List.iter st b
    | A.For (_, lo, hi, b) -> ex lo; ex hi; List.iter st b
    | A.Match (v, arms, other) -> ex v; List.iter (fun (_, b) -> List.iter st b) arms; Option.iter (List.iter st) other
    | A.Each (_, _, b) -> List.iter st b
    | _ -> ()
  in
  List.iter st body;
  !calls, !addrs

let clone_isr (tops : int A.top list) =
  let fns = List.filter_map (function A.Fn (f, _) -> Some (f.A.name, f) | _ -> None) tops in
  let calls g = match List.assoc_opt g fns with Some f -> fst (refs f.A.body) | None -> [] in
  let rec reach seen = function
    | [] -> seen
    | g :: rest when List.mem g seen || not (List.mem_assoc g fns) -> reach seen rest
    | g :: rest -> reach (g :: seen) (calls g @ rest)
  in
  let hooks = List.sort_uniq compare (List.concat_map (fun (_, f) -> snd (refs f.A.body)) fns) |> List.filter (fun g -> List.mem_assoc g fns) in
  let main = reach [] [ "main" ] and isr = reach [] hooks in
  let both = List.filter (fun g -> List.mem g main) isr in
  if both = [] then tops
  else begin
    let to_isr g = if List.mem g both then g ^ ".isr" else g in
    let clones =
      List.filter_map
        (function
          | A.Fn (f, ln) when List.mem f.A.name both -> Some (A.Fn ({ f with A.name = f.A.name ^ ".isr"; body = List.map (map_s to_isr) f.body }, ln))
          | _ -> None)
        tops
    in
    (* 割り込みの側だけの関数は、呼び出しを複製へ付け替える。番地を取る所（&H_timiEntry）も、複製の方を指す *)
    let tops =
      List.map
        (function
          | A.Fn (f, ln) when List.mem f.A.name isr && not (List.mem f.A.name both) -> A.Fn ({ f with A.body = List.map (map_s to_isr) f.body }, ln)
          | A.Fn (f, ln) ->
            A.Fn ({ f with A.body = List.map (map_s (fun g -> if List.mem g hooks && List.mem g both then g ^ ".isr" else g)) f.body }, ln)
          | t -> t)
        tops
    in
    tops @ clones
  end

let program ~hl_sigs tops =
  let tops = program ~hl_sigs tops in
  if not (List.exists (function A.Fn (f, _) -> f.A.name = "main" | _ -> false) tops) then err 0 "main がない";
  clone_isr tops
