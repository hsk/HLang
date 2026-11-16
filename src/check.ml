(* 型検査。行番号付きの構文木を受け取り、型付きの構文木を返す。ついでに:
   - 定数式を畳んで数値にする
   - 変数名を関数の中で一意にする（後段はスコープを考えなくてよい）
   - u8 を u16 が要る場所で使ったら、広げる変換を入れる
   - 表[添字] をメモリの読み書きに直す
   - next(p) を「読む文」と「進める文」に分けて、文の前に出す
   - match に抜けがないことを確かめる
   - 再帰がないことを確かめる
   - wait を「yield を繰り返すループ」に直す *)

open Ast

type prog = {
  globals : (string * ty * int) list;                      (* 名前, 型, 初期値 *)
  arrays : (string * ty * int) list;                       (* RAM の配列: 名前, 要素の型, 個数 *)
  datas : (string * ty * int list) list;                   (* ROM の表: 名前, 要素の型, 値 *)
  luts : (string * ty * int list) list;                    (* 表にした関数: 名前, 戻りの型, 256 個の値 *)
  fns : ty fn list;
  pools : (string * int * int) list;                       (* プールのタスク: 名前, 個数, フレームのバイト数 *)
  exts : (string * ((ty * string) list * ty * int * string list)) list;
    (* 名前, (引数, 戻り, 番地, 壊すレジスタ) *)
}

type env = {
  consts : (string, int) Hashtbl.t;
  globs : (string, ty) Hashtbl.t;
  sigs : (string, ty list * ty) Hashtbl.t;
  tasks : (string, ty list) Hashtbl.t;                    (* タスク -> spawn の引数の型 *)
  mutable self : string;                                  (* プールのタスクの中なら、フレームを指す変数 *)
  mutable frame : (string * int) list;                    (* フレームに置く変数 -> 先頭からの位置 *)
  mutable fsize : int;
  mutable later : bool;                                   (* いまの文より後ろ（同じブロック）に yield がある *)
  fns : (string, unit) Hashtbl.t;                         (* 番地を取れる関数（引数なし） *)
  mutable task : bool;                                    (* いまタスクの本体を調べている *)
  mutable hooks : string list;                            (* & で番地を取られた関数。割り込みから呼ばれるとみなす *)
  enums : (string, (string * int) list) Hashtbl.t;        (* 列挙型 -> (名前, 値) *)
  variants : (string, string * int) Hashtbl.t;            (* 列挙の名前 -> (列挙型, 値) *)
  tables : (string, ty) Hashtbl.t;                        (* 表・配列 -> 要素の型 *)
  bytes : (string, int) Hashtbl.t;                        (* 表・配列 -> バイト数 *)
  structs : (string, (string * (ty * int)) list * int) Hashtbl.t;
    (* 構造体 -> ((フィールド, (型, 先頭からの位置)) の並び, 全体のバイト数) *)
  svars : (string, string) Hashtbl.t;                     (* 構造体の大域変数 -> 構造体の名前 *)
  mutable scope : (string * (string * ty)) list;          (* ソース上の名前 -> (一意な名前, 型) *)
  used : (string, unit) Hashtbl.t;                        (* この関数で使った名前 *)
  mutable ret : ty;
  mutable loops : int;
  mutable calls : string list;
  mutable pre : ty stmt list;                             (* next() が文の前に出す文（逆順） *)
  mutable nohoist : string option;                        (* next() を使えない場所なら、その場所の名前 *)
}

let mask = function U8 | I8 | Enum _ -> 0xff | U16 | I16 | Ptr _ -> 0xffff | Bool | Unit -> 1

let num = function U8 | U16 | I8 | I16 -> true | _ -> false

(* 2 つの数の型を合わせる: どちらかが 16bit なら 16bit、どちらかが符号付きなら符号付き *)
let join a b =
  match wide a || wide b, signed a || signed b with
  | false, false -> U8 | false, true -> I8 | true, false -> U16 | true, true -> I16

let valid env ln = function
  | Enum e when Hashtbl.mem env.structs e -> err ln "構造体 %s はここでは *%s として使う" e e
  | Enum e when not (Hashtbl.mem env.enums e) -> err ln "知らない型: %s" e
  | Ptr s when not (Hashtbl.mem env.structs s) -> err ln "知らない構造体: %s" s
  | _ -> ()

(* 表の要素の型が構造体なら、その名前 *)
let struct_of env = function Enum s when Hashtbl.mem env.structs s -> Some s | _ -> None

(* コンパイル時に値が決まる式なら、その値 *)
let rec ceval env e =
  let ( let* ) = Option.bind in
  match e.x with
  | Int n -> Some n
  | Var v when not (List.mem_assoc v env.scope) -> Hashtbl.find_opt env.consts v
  | Un ("-", a) -> let* a = ceval env a in Some (-a)
  | Un ("~", a) -> let* a = ceval env a in Some (lnot a)
  | Bin (op, a, b) ->
    let* a = ceval env a in
    let* b = ceval env b in
    (match op with
     | "+" -> Some (a + b)
     | "-" -> Some (a - b)
     | "*" -> Some (a * b)
     | "/" when b <> 0 -> Some (a / b)
     | "%" when b <> 0 -> Some (a mod b)
     | "&" -> Some (a land b)
     | "|" -> Some (a lor b)
     | "^" -> Some (a lxor b)
     | "<<" -> Some (a lsl b)
     | ">>" -> Some (a asr b)
     | _ -> None)
  | _ -> None

(* 式を見ただけで分かる型。数値定数だけの式は None（文脈で決まる）。 *)
let rec hint env e =
  match e.x with
  | Int _ -> None
  | Tf _ -> Some Bool
  | Str _ | Addr _ -> Some U16
  | Glob _ -> None
  | Var v ->
    (match List.assoc_opt v env.scope with
     | Some (_, t) -> Some t
     | None ->
       if Hashtbl.mem env.globs v then Hashtbl.find_opt env.globs v
       else if Hashtbl.mem env.variants v then Some (Enum (fst (Hashtbl.find env.variants v)))
       else if Hashtbl.mem env.svars v then Some (Ptr (Hashtbl.find env.svars v))
       else if Hashtbl.mem env.tables v then Some U16
       else None)
  | Index ({ x = Var v; _ }, _) ->
    (match Hashtbl.find_opt env.tables v with
     | Some t -> (match struct_of env t with Some s -> Some (Ptr s) | None -> Some t)
     | None -> None)
  | Index _ -> None
  | Field (e, f) ->
    (match hint env e with
     | Some (Ptr s) ->
       Option.map fst (List.assoc_opt f (fst (Hashtbl.find env.structs s)))
     | _ -> None)
  | Lut _ -> None
  | Bin (("==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"), _, _) | Un ("!", _) -> Some Bool
  | Bin (("<<" | ">>"), l, _) | Un (_, l) -> hint env l
  | Bin (_, l, r) ->
    (match hint env l, hint env r with
     | Some a, Some b when num a && num b -> Some (join a b)
     | Some U16, _ | _, Some U16 -> Some U16
     | Some t, _ | _, Some t -> Some t
     | None, None -> None)
  | Call ("next", _) when not (Hashtbl.mem env.sigs "next") -> Some U8
  | Call ("next16", _) when not (Hashtbl.mem env.sigs "next16") -> Some U16
  | Call (f, _) -> Option.map snd (Hashtbl.find_opt env.sigs f)
  | Mem (t, _) | Cast (t, _) -> Some t
  | Port _ -> Some U8
  | Running _ -> Some Bool
  | Spawn (t, _) -> Some (if Hashtbl.mem env.structs t then Ptr t else Unit)

let log2 n =
  let rec go k = if 1 lsl k = n then Some k else if 1 lsl k > n then None else go (k + 1) in
  if n > 0 then go 0 else None

let rec yields ss =                              (* 中に yield か wait があるか *)
  List.exists
    (function
      | Yield | Wait _ -> true
      | If (_, t, f) -> yields t || yields f
      | While (_, b) | For (_, _, _, b) | Each (_, _, b) -> yields b
      | Match (_, arms, other) -> List.exists (fun (_, b) -> yields b) arms || yields (Option.value ~default:[] other)
      | _ -> false)
    ss

(* 変数を宣言し、一意な名前を返す *)
let declare env x t =
  let rec pick i =
    let u = if i = 0 then x else Printf.sprintf "%s_%d" x i in
    if Hashtbl.mem env.used u then pick (i + 1) else u
  in
  let u = pick 0 in
  Hashtbl.replace env.used u ();
  env.scope <- (x, (u, t)) :: env.scope;
  u

(* プールのタスクの変数をフレームに置く。個体ごとに別の値を持てる。 *)
let field env x t =
  let u = declare env x t in
  env.frame <- (u, env.fsize) :: env.frame;
  env.fsize <- env.fsize + (if wide t then 2 else 1);
  u

(* フレームに置いた変数は「self + 位置」のメモリとして読み書きする *)
let var env u t =
  match List.assoc_opt u env.frame with
  | None -> { x = Var u; a = t }
  | Some off ->
    let self = { x = Var env.self; a = U16 } in
    { x = Mem (t, { x = Bin ("+", self, { x = Int off; a = U16 }); a = U16 }); a = t }

(* want: この場所に要る型。None なら式から決める。 *)
let rec chk env want e =
  let ln = e.a in
  let num_ty es =                                  (* 数値演算をする幅 *)
    match want with
    | Some t when num t -> t
    | _ ->
      if List.exists (fun e -> match hint env e with Some (Bool | Enum _) -> true | _ -> false) es
      then err ln "数値が必要"
      else List.fold_left (fun t e -> match hint env e with Some h when num h -> join t h | _ -> t) U8 es
  in
  match ceval env e with
  | Some n ->
    let t = match want with
      | Some t when num t -> t
      | Some t -> err ln "%s が必要な場所に数値がある" (show_ty t)
      | None -> if n >= -128 && n < 256 then U8 else U16
    in
    if n > mask t || n < -(mask t + 1) / 2 then err ln "%d は %s に入らない" n (show_ty t);
    { x = Int (n land mask t); a = t }
  | None ->
    let r =
      match e.x with
      | Int _ | Glob _ | Lut _ | Running _ -> assert false
      | Addr v ->
        if Hashtbl.mem env.fns v then (env.hooks <- v :: env.hooks; { x = Addr v; a = U16 })
        else if Hashtbl.mem env.tables v then { x = Addr v; a = U16 }
        else if Hashtbl.mem env.svars v then { x = Addr v; a = Ptr (Hashtbl.find env.svars v) }
        else err ln "& で番地を取れるのは、引数のない関数と、表・配列だけ: %s" v
      | Call ("running", [ { x = Var t; _ } ]) when Hashtbl.mem env.tasks t -> { x = Running t; a = Bool }
      | Spawn (t, args) ->
        (* プールなら、始めた個体を指す値になる（空きがなければ、動かない予備の枠を指す） *)
        let ps = match Hashtbl.find_opt env.tasks t with Some ps -> ps | None -> err ln "知らないタスク: %s" t in
        if List.length ps <> List.length args then err ln "%s の引数は %d 個" t (List.length ps);
        let args = List.map2 (fun t a -> chk env (Some t) a) ps args in
        { x = Spawn (t, args); a = (if Hashtbl.mem env.structs t then Ptr t else Unit) }
      | Tf b -> { x = Tf b; a = Bool }
      | Str s -> { x = Str s; a = U16 }
      | Var v ->
        (match List.assoc_opt v env.scope with
         | Some (u, t) -> var env u t
         | None ->
           if Hashtbl.mem env.globs v then { x = Glob v; a = Hashtbl.find env.globs v }
           else if Hashtbl.mem env.variants v then
             (let en, n = Hashtbl.find env.variants v in { x = Int n; a = Enum en })
           else if Hashtbl.mem env.svars v then { x = Addr v; a = Ptr (Hashtbl.find env.svars v) }
           else if Hashtbl.mem env.tables v then { x = Addr v; a = U16 }
           else err ln "知らない名前: %s" v)
      | Index ({ x = Var v; _ }, i) when Hashtbl.mem env.tables v && not (List.mem_assoc v env.scope) ->
        (* 表[添字] は「先頭の番地 + 添字 × 要素の大きさ」を読む *)
        let t = Hashtbl.find env.tables v in
        let base = { x = Addr v; a = U16 } in
        let small = Hashtbl.find env.bytes v <= 256 && hint env i <> Some U16 && ceval env i = None in
        (match struct_of env t with
         | Some s ->
           (* 構造体の配列: 要素の番地（ポインタ）になる。添字に要素の大きさを掛ける。 *)
           let size = snd (Hashtbl.find env.structs s) in
           let scale t i =
             match log2 size with
             | Some 0 -> i
             | Some k -> { x = Bin ("<<", i, { x = Int k; a = U8 }); a = t }
             | None -> { x = Bin ("*", i, { x = Int size; a = t }); a = t }
           in
           let off =
             if small then { x = Cast (U16, scale U8 (chk env (Some U8) i)); a = U16 }
             else scale U16 (chk env (Some U16) i)
           in
           { x = Bin ("+", base, off); a = Ptr s }
         | None ->
           (* 256 バイト以下の表なら、添字は 8bit で計算する（番地の下位バイトに足すだけで済む） *)
           let i = if small then { x = Cast (U16, chk env (Some U8) i); a = U16 } else chk env (Some U16) i in
           let off = if wide t then { x = Bin ("<<", i, { x = Int 1; a = U8 }); a = U16 } else i in
           { x = Mem (t, { x = Bin ("+", base, off); a = U16 }); a = t })
      | Index _ -> err ln "添字が使えるのは表と配列だけ"
      | Field (e, f) ->
        (* p.f は「p の番地 + フィールドの位置」を読む *)
        let e = chk env None e in
        let s = match e.a with Ptr s -> s | t -> err ln "%s にフィールドはない" (show_ty t) in
        let t, off = match List.assoc_opt f (fst (Hashtbl.find env.structs s)) with
          | Some x -> x
          | None -> err ln "%s に %s というフィールドはない" s f
        in
        let at = if off = 0 then e else { x = Bin ("+", e, { x = Int off; a = U16 }); a = U16 } in
        { x = Mem (t, at); a = t }
      | Bin ((("&&" | "||") as o), l, r) ->
        let l = chk env (Some Bool) l in
        let saved = env.nohoist in
        env.nohoist <- Some "&& や || の右側";    (* 右側は実行されないことがある *)
        let r = chk env (Some Bool) r in
        env.nohoist <- saved;
        { x = Bin (o, l, r); a = Bool }
      | Bin ((("==" | "!=" | "<" | "<=" | ">" | ">=") as o), l, r) ->
        let t =
          match hint env l, hint env r with
          | (Some ((Bool | Enum _ | Ptr _) as t), _ | _, Some ((Bool | Enum _ | Ptr _) as t))
            when o = "==" || o = "!=" -> t
          | Some a, Some b when num a && num b -> join a b       (* どちらかが符号付きなら、符号付きで比べる *)
          | Some a, None | None, Some a when num a -> a
          | _ -> U8
        in
        let l = chk env (Some t) l in
        { x = Bin (o, l, chk env (Some t) r); a = Bool }
      | Bin ((("<<" | ">>") as o), l, r) ->
        let n = match ceval env r with
          | Some n when n >= 0 && n < 16 -> n
          | _ -> err ln "シフトする量は 0〜15 の定数だけ"
        in
        let l = chk env (Some (num_ty [ l ])) l in
        { x = Bin (o, l, { x = Int n; a = U8 }); a = l.a }
      | Bin ("*", l, r) ->
        (* 片方が定数の掛け算だけ。2 のべき乗ならシフトに直す。定数は右に寄せる。 *)
        let l, n = match ceval env l, ceval env r with
          | _, Some n when n >= 0 -> l, n
          | Some n, _ when n >= 0 -> r, n
          | _ -> err ln "掛け算は、片方が 0 以上の定数のときだけ"
        in
        let l = chk env (Some (num_ty [ l ])) l in
        (match log2 n with
         | Some k -> { x = Bin ("<<", l, { x = Int k; a = U8 }); a = l.a }
         | None -> { x = Bin ("*", l, { x = Int (n land mask l.a); a = l.a }); a = l.a })
      | Bin ((("/" | "%") as o), l, r) ->
        (* 符号なしの数を 2 のべき乗で割るならシフトとマスクで済む。それ以外は実行時のルーチンを呼ぶ。 *)
        let t = num_ty [ l; r ] in
        let l = chk env (Some t) l in
        (* 符号付きの割り算は 0 の方へ丸めるので、シフト（小さい方へ丸める）にはできない *)
        (match (if signed t then None else Option.bind (ceval env r) log2) with
         | Some k when o = "/" -> { x = Bin (">>", l, { x = Int k; a = U8 }); a = t }
         | Some k -> { x = Bin ("&", l, { x = Int ((1 lsl k) - 1); a = t }); a = t }
         | None -> { x = Bin (o, l, chk env (Some t) r); a = t })
      | Bin (o, l, r) ->
        let t = num_ty [ l; r ] in
        let l = chk env (Some t) l in
        { x = Bin (o, l, chk env (Some t) r); a = t }
      | Un ("!", a) -> { x = Un ("!", chk env (Some Bool) a); a = Bool }
      | Un (o, a) ->
        let t = num_ty [ a ] in
        { x = Un (o, chk env (Some t) a); a = t }
      | Call ((("next" | "next16") as f), args) when not (Hashtbl.mem env.sigs f) ->
        (* next(p): p の指すバイトを読んで p を進める。読む文と進める文を、いまの文の前に出す。 *)
        (match env.nohoist with Some where -> err ln "%s は%sでは使えない" f where | None -> ());
        let p = match args with
          | [ ({ x = Var _; _ } as p) ] -> chk env None p
          | _ -> err ln "%s の引数は u16 の変数 1 つ" f
        in
        (match p.x, p.a with
         | (Var _ | Glob _ | Mem _), U16 -> ()
         | _ -> err ln "%s の引数は u16 の変数 1 つ" f);
        let t, n = if f = "next" then U8, 1 else U16, 2 in
        let tmp = declare env "_" t in
        let bump = { x = Bin ("+", p, { x = Int n; a = U16 }); a = U16 } in
        env.pre <- Set (p, bump) :: Let (tmp, Some t, { x = Mem (t, p); a = t }) :: env.pre;
        { x = Var tmp; a = t }
      | Call (f, args) ->
        let ps, ret = match Hashtbl.find_opt env.sigs f with
          | Some s -> s
          | None -> err ln "知らない関数: %s" f
        in
        if List.length ps <> List.length args then
          err ln "%s の引数は %d 個" f (List.length ps);
        env.calls <- f :: env.calls;
        { x = Call (f, List.map2 (fun t a -> chk env (Some t) a) ps args); a = ret }
      | Mem (t, a) -> { x = Mem (t, chk env (Some U16) a); a = t }
      | Port p ->
        (match ceval env p with
         | Some n when n >= 0 && n < 256 -> { x = Port { x = Int n; a = U8 }; a = U8 }
         | _ -> err ln "ポート番号は 0〜255 の定数だけ")
      | Cast (t, a) ->
        valid env ln t;
        let a = chk env None a in
        if a.a = Unit then err ln "値のない式は変換できない";
        if a.a = t then a else { x = Cast (t, a); a = t }
    in
    (match want, r.a with
     | None, _ -> r
     | Some t, t' when t = t' -> r
     (* 数どうしは自動で合わせる。幅が同じならビットはそのまま、広げるときは元の型の符号に従う *)
     | Some t, t' when num t && num t' && (wide t || not (wide t')) -> { x = Cast (t, r); a = t }
     | Some t, t' ->
       err ln "%s が必要な場所に %s がある%s" (show_ty t) (show_ty t')
         (if t' = U16 || (match t' with Enum _ -> true | _ -> false)
          then Printf.sprintf "（as %s で変換できる）" (show_ty t) else ""))

(* next() が出した文を取り出す *)
let take env =
  let p = List.rev env.pre in
  env.pre <- [];
  p

let rec block env ln ss =
  let saved = env.scope and later = env.later in
  let rec go = function
    | [] -> []
    | s :: rest ->
      env.later <- yields rest;
      let r = stmt env ln s in
      r @ go rest
  in
  let r = go ss in
  env.scope <- saved;
  env.later <- later;
  r

(* 文 1 つ。先頭の式を調べ、next() が出した文があればその前に置く。
   中に文を持つもの（if など）は、先頭の式を調べ終えてから中身に進む。 *)
and stmt env ln s : ty stmt list =
  let keep = env.self <> "" && env.later in       (* 新しい変数を、中断をまたいで持ち越すことがあるか *)
  let in_loop f =
    env.loops <- env.loops + 1;
    let r = f () in
    env.loops <- env.loops - 1;
    r
  in
  let rest : unit -> ty stmt =
    match s with
    | Let (x, t, e) ->
      Option.iter (valid env ln) t;
      let e = chk env t e in
      if e.a = Unit then err ln "値のない式で変数は作れない: %s" x;
      if keep then (let u = field env x e.a in fun () -> Set (var env u e.a, e))
      else (let u = declare env x e.a in fun () -> Let (u, Some e.a, e))
    | Set (l, e) ->
      let l = match l.x with
        | Var _ | Mem _ | Port _ | Index _ | Field _ ->
          if ceval env l <> None then err l.a "定数には代入できない";
          chk env None l
        | _ -> err l.a "代入できない式"
      in
      (match l.x with Var _ | Glob _ | Mem _ | Port _ -> () | _ -> err ln "代入できない式");
      let e = chk env (Some l.a) e in
      fun () -> Set (l, e)
    | If (c, t, f) ->
      let c = chk env (Some Bool) c in
      fun () -> let t = block env ln t in If (c, t, block env ln f)
    | While (c, b) ->
      env.nohoist <- Some "while の条件";          (* 毎回調べ直す式なので、前には出せない *)
      let c = chk env (Some Bool) c in
      env.nohoist <- None;
      fun () -> While (c, in_loop (fun () -> block env ln b))
    | For (x, lo, hi, b) ->
      let t = if hint env lo = Some U16 || hint env hi = Some U16 then U16 else U8 in
      let lo = chk env (Some t) lo in
      let hi = chk env (Some t) hi in
      if env.self <> "" && yields b then err ln "プールのタスクでは、yield を含む for は書けない（while で書く）";
      fun () ->
        let saved = env.scope in
        let u = declare env x t in
        let b = in_loop (fun () -> block env ln b) in
        env.scope <- saved;
        For (u, lo, hi, b)
    | Match (e, arms, other) ->
      let e = chk env None e in
      (match e.a with
       | U8 | Enum _ -> ()
       | t -> err ln "match で調べられるのは u8 か列挙型（%s は不可）" (show_ty t));
      fun () ->
        let seen = ref [] in
        let arms =
          List.map
            (fun (ps, b) ->
              let ps =
                List.map
                  (fun (p : int expr) ->
                    let q = chk env (Some e.a) p in
                    match q.x with
                    | Int n ->
                      if List.mem n !seen then err p.a "match に同じ値が 2 回ある";
                      seen := n :: !seen;
                      q
                    | _ -> err p.a "match の値は定数だけ")
                  ps
              in
              ps, block env ln b)
            arms
        in
        let other = Option.map (block env ln) other in
        (* 網羅性: _ がないなら、列挙型のすべての値が書いてあること *)
        (if other = None then
           match e.a with
           | Enum en ->
             let missing = List.filter (fun (_, n) -> not (List.mem n !seen)) (Hashtbl.find env.enums en) in
             if missing <> [] then
               err ln "match に抜けがある: %s" (String.concat ", " (List.map fst missing))
           | _ -> err ln "u8 の match には _ => が必要");
        Match (e, arms, other)
    | (Break | Continue) as s ->
      if env.loops = 0 then err ln "ループの外では使えない";
      fun () -> s
    | Return None ->
      if env.ret <> Unit then err ln "戻り値が必要";
      fun () -> Return None
    | Return (Some e) ->
      if env.ret = Unit then err e.a "この関数は値を返さない";
      let e = chk env (Some env.ret) e in
      fun () -> Return (Some e)
    | Yield ->
      if not env.task then err ln "yield は task の中でだけ使える";
      fun () -> Yield
    | Wait e ->
      (* wait n は「n 回 yield する」。回数を変数に取って、while に直す。 *)
      if not env.task then err ln "wait は task の中でだけ使える";
      let e = chk env (Some U8) e in
      let int k = { x = Int k; a = U8 } in
      let n =
        if env.self <> "" then (let n = var env (field env "_" U8) U8 in env.pre <- Set (n, e) :: env.pre; n)
        else (let u = declare env "_" U8 in env.pre <- Let (u, Some U8, e) :: env.pre; { x = Var u; a = U8 })
      in
      fun () ->
        While ({ x = Bin ("!=", n, int 0); a = Bool }, [ Yield; Set (n, { x = Bin ("-", n, int 1); a = U8 }) ])
    | Resume t ->
      if not (Hashtbl.mem env.tasks t) then err ln "知らないタスク: %s" t;
      env.calls <- t :: env.calls;
      fun () -> Resume t
    | Each (x, t, b) ->
      (* 動いている個体を順に見る。x は個体を指す値で、x.引数名 で読み書きできる *)
      if not (Hashtbl.mem env.structs t && Hashtbl.mem env.tasks t) then err ln "for ... in で回れるのは pool の付いたタスクだけ: %s" t;
      if yields b then err ln "for ... in の中では yield できない";
      fun () ->
        let saved = env.scope in
        let u = declare env x (Ptr t) in
        let b = in_loop (fun () -> block env ln b) in
        env.scope <- saved;
        Each (u, t, b)
    | Stop (_, Some { x = Var t; _ }) when Hashtbl.mem env.tasks t && not (List.mem_assoc t env.scope) ->
      fun () -> Stop (t, None)                       (* タスクを丸ごと止める *)
    | Stop (_, Some e) ->
      let e = chk env None e in
      (match e.a with
       | Ptr t when Hashtbl.mem env.tasks t -> fun () -> Stop (t, Some e)
       | t -> err ln "stop できるのはタスクか、その個体（%s は不可）" (show_ty t))
    | Stop (_, None) -> assert false
    | Do e ->
      (match e.x with
       | Call _ | Spawn _ -> let e = chk env None e in fun () -> Do e
       | _ -> err e.a "この式は文として使えない")
  in
  let pre = take env in
  pre @ [ rest () ]

(* どの道を通っても return するか *)
let rec returns ss =
  match List.rev ss with
  | Return _ :: _ -> true
  | If (_, t, f) :: _ -> returns t && returns f
  | Match (_, arms, other) :: _ ->
    List.for_all (fun (_, b) -> returns b) arms && (match other with Some b -> returns b | None -> true)
  | While ({ x = Tf true; _ }, _) :: _ -> true
  | _ -> false

let check (tops : int top list) : prog =
  let env = {
    consts = Hashtbl.create 16; globs = Hashtbl.create 16; sigs = Hashtbl.create 16;
    tasks = Hashtbl.create 16; fns = Hashtbl.create 16; task = false; hooks = [];
    self = ""; frame = []; fsize = 0; later = false;
    enums = Hashtbl.create 16; variants = Hashtbl.create 16; tables = Hashtbl.create 16;
    bytes = Hashtbl.create 16; structs = Hashtbl.create 16; svars = Hashtbl.create 16;
    scope = []; used = Hashtbl.create 16; ret = Unit; loops = 0; calls = [];
    pre = []; nohoist = Some "ここ";
  } in
  let builtin = Hashtbl.create 8 in                (* 組み込みの名前。同じ名前の関数を自分で定義すれば、そちらが使われる *)
  let define ln name =
    let taken = Hashtbl.mem env.sigs name && not (Hashtbl.mem builtin name) in
    Hashtbl.remove builtin name;
    if Hashtbl.mem env.consts name || Hashtbl.mem env.globs name || taken
       || Hashtbl.mem env.variants name || Hashtbl.mem env.tables name || Hashtbl.mem env.svars name
       || Hashtbl.mem env.tasks name
    then err ln "名前が重複: %s" name
  in
  let const ln e =
    match ceval env e with Some n -> n | None -> err ln "定数式が必要"
  in
  let value t (e : int expr) =                     (* 型 t の定数 *)
    match (chk env (Some t) e).x with
    | Int n -> n
    | Tf b -> Bool.to_int b
    | _ -> err e.a "定数式が必要"
  in
  (* 1 命令だけの組み込み: 割り込みの許可と禁止、割り込みを待つ *)
  List.iter (fun f -> Hashtbl.replace builtin f (); Hashtbl.replace env.sigs f ([], Unit)) Asm.bare;
  List.iter
    (fun (f, (regs, _, _)) ->
      Hashtbl.replace builtin f ();
      Hashtbl.replace env.sigs f (List.map (fun r -> if String.length r = 2 then U16 else U8) regs, if f = "inp" then U8 else Unit))
    Asm.prims;
  (* 1 回目: 名前と型を集める。関数は定義より前で呼べる。 *)
  let globals = ref [] and exts = ref [] and arrays = ref [] and datas = ref [] in
  List.iter
    (function
      | Const (x, e) -> define e.a x; Hashtbl.replace env.consts x (const e.a e)
      | EnumDef (name, vs, ln) ->
        if Hashtbl.mem env.enums name then err ln "名前が重複: %s" name;
        let next = ref 0 in
        let vs =
          List.map
            (fun (v, e) ->
              define ln v;
              let n = match e with Some e -> const ln e | None -> !next in
              if n < 0 || n > 255 then err ln "%s の値 %d は 1 バイトに入らない" v n;
              next := n + 1;
              Hashtbl.replace env.variants v (name, n);
              v, n)
            vs
        in
        Hashtbl.replace env.enums name vs
      | StructDef (name, fs, ln) ->
        if Hashtbl.mem env.enums name || Hashtbl.mem env.structs name then err ln "名前が重複: %s" name;
        (* フィールドを書いた順に並べる。順に読み書きすれば HL を進めるだけで歩ける。 *)
        let fields, size =
          List.fold_left
            (fun (l, off) (f, t) ->
              (match t with Ptr _ -> () | t -> valid env ln t);
              if List.mem_assoc f l then err ln "フィールドが重複: %s" f;
              l @ [ f, (t, off) ], off + if wide t then 2 else 1)
            ([], 0) fs
        in
        Hashtbl.replace env.structs name (fields, size)
      | Global (x, Enum s, None) when Hashtbl.mem env.structs s ->
        (* 構造体の大域変数。名前はその番地（ポインタ）になる。 *)
        define 0 x;
        Hashtbl.replace env.svars x s;
        arrays := (x, U8, snd (Hashtbl.find env.structs s)) :: !arrays
      | Array (x, Enum s, n, ln) when Hashtbl.mem env.structs s ->
        define ln x;
        let n = const ln n in
        if n <= 0 then err ln "配列の個数は 1 以上";
        let bytes = n * snd (Hashtbl.find env.structs s) in
        Hashtbl.replace env.tables x (Enum s);
        Hashtbl.replace env.bytes x bytes;
        arrays := (x, U8, bytes) :: !arrays
      | Global (x, t, init) ->
        define 0 x;
        valid env 0 t;
        let v = match init with None -> 0 | Some e -> value t e in
        Hashtbl.replace env.globs x t;
        globals := (x, t, v) :: !globals
      | Array (x, t, n, ln) ->
        define ln x;
        valid env ln t;
        let n = const ln n in
        if n <= 0 then err ln "配列の個数は 1 以上";
        Hashtbl.replace env.tables x t;
        Hashtbl.replace env.bytes x (n * if wide t then 2 else 1);
        arrays := (x, t, n) :: !arrays
      | Data (x, t, es, ln) ->
        define ln x;
        valid env ln t;
        (* u8 の表には列挙の名前も書ける。バイトコードを書くのに使う。 *)
        let elem (e : int expr) =
          match t, e.x with
          | U8, Var v when Hashtbl.mem env.variants v -> snd (Hashtbl.find env.variants v)
          | _ -> value t e
        in
        Hashtbl.replace env.tables x t;
        Hashtbl.replace env.bytes x (List.length es * if wide t then 2 else 1);
        datas := (x, t, List.map elem es) :: !datas
      | Import (f, ln) -> err ln "import できない: %s" f
      | Fn (f, ln) when f.task ->
        define ln f.name;
        List.iter (fun (_, t) -> valid env ln t) f.ps;
        (* プールのタスクは、引数をフィールドに持つ構造体としても見える。個体を指す値は *タスク名 *)
        if f.pool > 0 then begin
          if Hashtbl.mem env.enums f.name || Hashtbl.mem env.structs f.name then err ln "名前が重複: %s" f.name;
          let fields, size =
            List.fold_left (fun (l, off) (x, t) -> l @ [ x, (t, off) ], off + if wide t then 2 else 1) ([], 0) f.ps
          in
          Hashtbl.replace env.structs f.name (fields, size)
        end;
        Hashtbl.replace env.tasks f.name (List.map snd f.ps)
      | Fn (f, ln) ->
        define ln f.name;
        if f.ps = [] && not f.lut then Hashtbl.replace env.fns f.name ();
        List.iter (fun (_, t) -> valid env ln t) f.ps;
        valid env ln f.ret;
        Hashtbl.replace env.sigs f.name (List.map snd f.ps, f.ret)
      | Extern (name, ps, ret, addr, clob, ln) ->
        define ln name;
        List.iter
          (fun (t, r) ->
            let ok = match t with
              | U16 | I16 | Ptr _ -> List.mem r [ "hl"; "de"; "bc" ]
              | _ -> List.mem r [ "a"; "b"; "c"; "d"; "e"; "h"; "l" ]
            in
            if not ok then err ln "%s は %s を渡せるレジスタではない" r (show_ty t))
          ps;
        Hashtbl.replace env.sigs name (List.map fst ps, ret);
        let clob = match clob with
          | None -> Asm.all8
          | Some rs ->
            List.concat_map
              (fun r -> match Asm.halves r with [] -> err ln "知らないレジスタ: %s" r | l -> l)
              rs
            @ (match ret with Unit -> [] | U16 | I16 | Ptr _ -> [ "h"; "l" ] | _ -> [ "a" ])
        in
        exts := (name, (ps, ret, const ln addr, clob)) :: !exts)
    tops;
  env.nohoist <- None;
  (* 2 回目: 関数の本体 *)
  let graph = ref [] and pools = ref [] in
  let fns =
    List.filter_map
      (function
        | Fn (f, ln) ->
          env.scope <- [];
          Hashtbl.reset env.used;
          env.ret <- f.ret;
          env.task <- f.task;
          env.calls <- [];
          env.frame <- [];
          env.fsize <- 0;
          env.self <- "";
          (* プールのタスク: 引数はフレームの先頭に並ぶ。spawn がそこへ書く。 *)
          if f.pool > 0 then env.self <- declare env "self" U16;
          let ps = List.map (fun (x, t) -> (if f.pool > 0 then field env x t else declare env x t), t) f.ps in
          let body = block env ln f.body in
          if f.ret <> Unit && not (returns body) then err ln "%s: return しない道がある" f.name;
          if env.fsize > 120 then err ln "%s: 中断をまたぐ変数が多すぎる（%d バイト。120 まで）" f.name env.fsize;
          if f.pool > 0 then pools := (f.name, f.pool, env.fsize + 2) :: !pools;
          graph := (f.name, env.calls) :: !graph;
          let self = env.self in
          env.self <- "";
          Some { f with ps; body; self }
        | _ -> None)
      tops
  in
  (* 再帰の検査: 呼び出しをたどって自分に戻らないこと *)
  let rec visit path f =
    if List.mem f path then
      err 0 "再帰は使えない: %s" (String.concat " -> " (List.rev (f :: path)));
    List.iter (visit (f :: path)) (Option.value ~default:[] (List.assoc_opt f !graph))
  in
  List.iter (fun (f, _) -> visit [] f) !graph;
  if not (Hashtbl.mem env.sigs "main") then err 0 "main がない";
  (* 変数の置き場所は固定なので、割り込みが普段の処理と同じ関数に入ると、使っている最中の変数を壊す。
     & で番地を取った関数は割り込みから呼ばれるとみなし、そこから届く関数を main からも呼んでいないか調べる。 *)
  let rec reach seen f =
    if List.mem f seen || not (List.mem_assoc f !graph) then seen
    else List.fold_left reach (f :: seen) (List.assoc f !graph)
  in
  let normal = reach [] "main" in
  List.iter
    (fun h ->
      match List.filter (fun f -> List.mem f normal) (reach [] h) with
      | [] -> ()
      | both ->
        err 0 "%s は割り込みから（&%s 経由で）も、普段の処理（main）からも呼ばれる。変数が壊れるので、どちらか専用にする"
          (String.concat ", " (List.rev both)) h)
    env.hooks;
  { globals = List.rev !globals; arrays = List.rev !arrays; datas = List.rev !datas; luts = [];
    fns; exts = List.rev !exts; pools = List.rev !pools }

(* for e in タスク を、普通の文に直す。フレームを先頭から順に見て、空きでないものだけ本体を実行する。
   増やすのを先頭でやるので、本体の continue はそのまま次の個体へ進む。 *)
let each (pools : (string * int * int) list) x t body : ty stmt list =
  let _, n, size = List.find (fun (u, _, _) -> u = t) pools in
  let u16 x = { x; a = U16 } and u8 x = { x; a = U8 } in
  let e = { x = Var x; a = Ptr t } and i = u8 (Var (x ^ ".left")) in
  let int k = u16 (Int k) in
  [ Let (x, Some (Ptr t), { x = Cast (Ptr t, u16 (Bin ("+", u16 (Addr (t ^ ".pool")), int ((2 - size) land 0xffff)))); a = Ptr t });
    Let (x ^ ".left", Some U8, u8 (Int n));
    While ({ x = Bin ("!=", i, u8 (Int 0)); a = Bool },
           [ Set (i, u8 (Bin ("-", i, u8 (Int 1))));
             Set (e, { x = Cast (Ptr t, u16 (Bin ("+", u16 (Cast (U16, e)), int size))); a = Ptr t });
             If ({ x = Bin ("!=", u8 (Mem (U8, u16 (Bin ("-", u16 (Cast (U16, e)), int 1)))), u8 (Int 0)); a = Bool }, body, []) ]) ]
