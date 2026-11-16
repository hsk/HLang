(* 構文木。式には注釈 'a が付く。構文解析の直後は行番号、型検査の後は型になる。 *)

type ty =
  | U8
  | U16
  | I8                           (* 符号付き。足し引きは符号なしと同じビット演算で、比べ方と広げ方だけが違う *)
  | I16
  | Bool
  | Unit
  | Enum of string               (* 列挙型は 1 バイト。構文解析の直後は、構造体の名前もこれで表す *)
  | Ptr of string                (* 構造体を指す番地 *)

let wide = function U16 | I16 | Ptr _ -> true | _ -> false     (* 2 バイトの型か *)
let signed = function I8 | I16 -> true | _ -> false

type 'a expr = { x : 'a desc; a : 'a }

and 'a desc =
  | Int of int
  | Tf of bool
  | Str of string                              (* 文字列の置き場所の番地 *)
  | Var of string
  | Glob of string                             (* 型検査が Var から振り分ける *)
  | Addr of string                             (* 表や配列の先頭の番地 *)
  | Index of 'a expr * 'a expr                 (* 表[添字]。型検査が Mem に直す *)
  | Field of 'a expr * string                  (* 構造体.フィールド。型検査が Mem に直す *)
  | Lut of string * 'a expr                    (* 表にした関数を引く。Lut モジュールが Call から直す *)
  | Bin of string * 'a expr * 'a expr
  | Un of string * 'a expr
  | Call of string * 'a expr list
  | Mem of ty * 'a expr                        (* mem[e], mem16[e] *)
  | Port of 'a expr                            (* port[n] *)
  | Cast of ty * 'a expr
  | Running of string                          (* running(タスク): まだ終わっていないか *)

type 'a stmt =
  | Let of string * ty option * 'a expr
  | Set of 'a expr * 'a expr
  | If of 'a expr * 'a stmt list * 'a stmt list
  | While of 'a expr * 'a stmt list
  | For of string * 'a expr * 'a expr * 'a stmt list   (* for i in lo..hi *)
  | Match of 'a expr * ('a expr list * 'a stmt list) list * 'a stmt list option
    (* 調べる式, (値の並び, 本体) の並び, どれでもないときの本体 *)
  | Break
  | Continue
  | Return of 'a expr option
  | Do of 'a expr
  | Yield                                      (* タスクを中断して、resume した側へ戻る *)
  | Wait of 'a expr                            (* n 回 yield する。型検査が while と yield に直す *)
  | Spawn of string * 'a expr list             (* タスクを最初から始める状態にする *)
  | Resume of string                           (* タスクを次の yield まで進める *)
  | Each of string * string * 'a stmt list     (* for e in タスク: 動いている個体を順に見る *)
  | Stop of string * 'a expr option            (* stop タスク / stop 個体: 止める *)

type 'a fn = {
  name : string;
  ps : (string * ty) list;
  ret : ty;
  body : 'a stmt list;
  lut : bool;                    (* lut fn: コンパイル時に全部の値を計算して表にする *)
  task : bool;                   (* task: 途中で中断して、後で続きから再開できる *)
  pool : int;                    (* task ... pool n: 同時に n 個まで動かせる。0 なら 1 個だけのタスク *)
  self : string;                 (* プールのタスクで、いま動いている個体のフレームを指す変数 *)
}

type 'a top =
  | Const of string * 'a expr
  | Global of string * ty * 'a expr option
  | Array of string * ty * 'a expr * int       (* var 名前: [型; 個数] — RAM 上の配列 *)
  | Data of string * ty * 'a expr list * int   (* data 名前: 型 = [値, ...] — ROM 上の表 *)
  | EnumDef of string * (string * 'a expr option) list * int
  | StructDef of string * (string * ty) list * int
  | Fn of 'a fn * int
  | Import of string * int                     (* import "ファイル" *)
  | Extern of string * (ty * string) list * ty * 'a expr * string list option * int
    (* 引数は (型, レジスタ)。最後のリストは壊すレジスタ。書かなければ全部壊すとみなす。 *)

exception Error of int * string                (* 行番号, 内容 *)

let err line fmt = Printf.ksprintf (fun s -> raise (Error (line, s))) fmt

let show_ty = function
  | U8 -> "u8" | U16 -> "u16" | I8 -> "i8" | I16 -> "i16" | Bool -> "bool" | Unit -> "()" | Enum e -> e | Ptr s -> "*" ^ s
