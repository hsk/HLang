(* 中間表現。関数と継続だけでできている。

   継続は引数を持つラベル。分岐もループも合流も「継続へ引数を渡して飛ぶ」で表す。
   変数は一度しか束縛されない。値が変わる変数は、継続の引数になる。 *)

type ty = U8 | U16

type atom =
  | V of string                  (* 変数 *)
  | C of int                     (* 定数 *)
  | S of string                  (* ラベルの番地 *)

type exp =
  | Atom of atom
  | Bin of string * atom * atom  (* + - & | ^ *)
  | Sh of string * atom * int    (* << >> を定数回 *)
  | Mulc of atom * int           (* 定数倍 *)
  | Idx of string * atom         (* 表の番地 + 8bit の添字。表はページをまたがない *)
  | Lut of string * atom         (* 256 バイト境界に置いた表を 8bit の添字で引く *)
  | Un of string * atom          (* - ~ *)
  | Zext of atom                 (* u8 -> u16 *)
  | Trunc of atom                (* u16 -> u8 *)
  | Cmp of string * ty * atom * atom   (* 比較の結果を 0/1 で *)
  | Load of atom                 (* メモリを読む *)
  | In of int                    (* ポートを読む *)

type term =
  | Let of string * ty * exp * term
  | Store of ty * atom * atom * term           (* 番地, 値 *)
  | Out of int * atom * term
  | Call of (string * ty) option * string * (atom * ty) list * term
  | LetK of kont list * term                   (* 継続を定義する。互いに呼び合える *)
  | Jump of string * atom list                 (* 継続へ飛ぶ *)
  | If of string * ty * atom * atom * term * term
  | Switch of atom * (int * string) list * string option
    (* 値で分岐: (値, 飛び先の継続) の並びと、どれでもないときの継続。飛び先は引数なし *)
  | Ret of (atom * ty) option
  | Yield of term                              (* タスクを中断する。再開したら続きへ *)

and kont = { k : string; ps : (string * ty) list; body : term }

type fn = {
  name : string;
  ps : (string * ty) list;
  ret : ty option;
  body : term;
  task : bool;
  self : string option;          (* プールのタスク: いまの個体のフレームを指す変数。入口と再開のたびに HL で届く *)
}

(* ---- 読める形にする ---- *)

let ty = function U8 -> "u8" | U16 -> "u16"
let atom = function V v -> v | C n -> string_of_int n | S s -> "&" ^ s
let atoms l = String.concat ", " (List.map atom l)
let params ps = String.concat ", " (List.map (fun (x, t) -> x ^ ": " ^ ty t) ps)

let exp = function
  | Atom a -> atom a
  | Bin (o, a, b) | Cmp (o, _, a, b) -> Printf.sprintf "%s %s %s" (atom a) o (atom b)
  | Sh (o, a, n) -> Printf.sprintf "%s %s %d" (atom a) o n
  | Mulc (a, n) -> Printf.sprintf "%s * %d" (atom a) n
  | Idx (s, a) -> Printf.sprintf "&%s[%s]" s (atom a)
  | Lut (s, a) -> Printf.sprintf "%s[%s]" s (atom a)
  | Un (o, a) -> o ^ atom a
  | Zext a -> "zext " ^ atom a
  | Trunc a -> "trunc " ^ atom a
  | Load a -> "mem[" ^ atom a ^ "]"
  | In p -> Printf.sprintf "port[%d]" p

let rec term ind t =
  let line fmt = Printf.ksprintf (fun s -> ind ^ s ^ "\n") fmt in
  match t with
  | Let (x, t, e, k) -> line "let %s: %s = %s" x (ty t) (exp e) ^ term ind k
  | Store (_, a, v, k) -> line "mem[%s] = %s" (atom a) (atom v) ^ term ind k
  | Out (p, v, k) -> line "port[%d] = %s" p (atom v) ^ term ind k
  | Call (r, f, args, k) ->
    let bind = match r with Some (x, t) -> Printf.sprintf "let %s: %s = " x (ty t) | None -> "" in
    line "%s%s(%s)" bind f (atoms (List.map fst args)) ^ term ind k
  | LetK (ks, body) ->
    term ind body
    ^ String.concat ""
        (List.map (fun k -> line "cont %s(%s) =" k.k (params k.ps) ^ term (ind ^ "  ") k.body) ks)
  | Jump (k, args) -> line "%s(%s)" k (atoms args)
  | If (o, _, a, b, t, f) ->
    line "if %s %s %s then" (atom a) o (atom b)
    ^ term (ind ^ "  ") t ^ line "else" ^ term (ind ^ "  ") f
  | Switch (a, cases, other) ->
    line "switch %s" (atom a)
    ^ String.concat "" (List.map (fun (n, k) -> line "  %d => %s()" n k) cases)
    ^ (match other with Some k -> line "  _ => %s()" k | None -> "")
  | Ret None -> line "ret()"
  | Ret (Some (a, _)) -> line "ret(%s)" (atom a)
  | Yield k -> line "yield" ^ term ind k

let show (f : fn) =
  Printf.sprintf "%s %s(%s)%s =\n%s" (if f.task then "task" else "fn") f.name (params f.ps)
    (match f.ret with Some t -> " -> " ^ ty t | None -> "")
    (term "  " f.body)
