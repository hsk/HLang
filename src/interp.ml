(* 参照インタプリタ。型付きの構文木をそのまま実行する。
   コンパイラが出した機械語と結果を比べるための、言語の意味の基準。 *)

open Ast

exception Out_of_fuel

type world = {
  mem : Bytes.t;                               (* mem[] で見える 64KB *)
  ext : string -> int list -> int;             (* extern 関数の呼び出し *)
  inp : int -> int;                            (* port[] の読み *)
  out : int -> int -> unit;                    (* port[] への書き *)
  mutable fuel : int;                          (* 無限ループ防止 *)
}

(* タスクの中断。エフェクトで「ここから先」を取っておき、resume で続きを走らせる。 *)
type _ Effect.t += Pause : unit Effect.t
type task = Idle | Fresh | Paused of (unit, unit) Effect.Deep.continuation

exception Brk
exception Cont
exception Ret of int

let mask = Check.mask
let str_base = 0x8000                          (* 文字列と表を置く場所（番地そのものに意味はない） *)
let array_base = 0xd000                        (* 配列を置く場所 *)

(* プログラムを用意し、「関数を呼ぶ関数」を返す *)
let load (p : Check.prog) w =
  let globs = Hashtbl.create 16 in
  let luts = Hashtbl.create 16 in
  List.iter (fun (f, _, vs) -> Hashtbl.replace luts f (Array.of_list vs)) p.luts;
  List.iter (fun (x, _, v) -> Hashtbl.replace globs x v) p.globals;
  let strs = Hashtbl.create 16 and top = ref str_base in
  let str s =
    match Hashtbl.find_opt strs s with
    | Some a -> a
    | None ->
      let a = !top in
      Bytes.blit_string (s ^ "\000") 0 w.mem a (String.length s + 1);
      top := a + String.length s + 1;
      Hashtbl.replace strs s a;
      a
  in
  (* 表と配列に番地を割り当てる *)
  let addrs = Hashtbl.create 16 in
  List.iter
    (fun (x, t, vs) ->
      Hashtbl.replace addrs x !top;
      List.iter
        (fun v ->
          Bytes.set w.mem !top (Char.chr (v land 0xff));
          incr top;
          if wide t then (Bytes.set w.mem !top (Char.chr (v lsr 8)); incr top))
        vs)
    p.datas;
  let atop = ref array_base in
  List.iter
    (fun (x, t, n) -> Hashtbl.replace addrs x !atop; atop := !atop + (n * if wide t then 2 else 1))
    p.arrays;
  List.iteri (fun i (f : ty fn) -> Hashtbl.replace addrs f.name (0x100 + i)) p.fns;   (* 関数の番地（仮） *)
  (* プールのフレームを置く場所。タスクの状態は (名前, 何番目か) ごとに持つ。 *)
  List.iter (fun (t, n, size) -> Hashtbl.replace addrs (t ^ ".pool") !atop; atop := !atop + ((n + 1) * size)) p.pools;   (* 最後の 1 つは予備の枠 *)
  let pool t = List.find_map (fun (u, n, size) -> if u = t then Some (n, size) else None) p.pools in
  let frame t i size = Hashtbl.find addrs (t ^ ".pool") + (i * size) + 2 in
  let slots t = match pool t with Some (n, _) -> List.init n Fun.id | None -> [ 0 ] in
  let tasks = Hashtbl.create 8 in
  let state t = Option.value ~default:Idle (Hashtbl.find_opt tasks t) in
  (* 状態を変える。プールなら、フレームの「続きの番地」の上位バイトにも、動いているかを書いておく
     （機械語ではそこが 0 かどうかで空きを見分ける。for e in タスク がそれを読む） *)
  let set (t, i) s =
    Hashtbl.replace tasks (t, i) s;
    match pool t with
    | Some (_, size) -> Bytes.set w.mem (frame t i size - 1) (if s = Idle then '\000' else '\001')
    | None -> ()
  in
  let burn () = w.fuel <- w.fuel - 1; if w.fuel < 0 then raise Out_of_fuel in
  let peek t a =
    let b i = Char.code (Bytes.get w.mem ((a + i) land 0xffff)) in
    if wide t then b 0 lor (b 1 lsl 8) else b 0
  in
  let poke t a v =
    let b i v = Bytes.set w.mem ((a + i) land 0xffff) (Char.chr (v land 0xff)) in
    b 0 v;
    if wide t then b 1 (v lsr 8)
  in
  let rec call f args =
    burn ();
    match List.find_opt (fun (g : ty fn) -> g.name = f) p.fns, f, args with
    | None, "copy", [ dst; src; n ] ->                (* ldir と同じく、前から 1 バイトずつ。0 個は 65536 個 *)
      for i = 0 to ((n - 1) land 0xffff) do poke U8 (dst + i) (peek U8 (src + i)) done;
      0
    | None, "outs", [ port; src; n ] ->
      for i = 0 to ((n - 1) land 0xff) do w.out port (peek U8 (src + i)) done;
      0
    | None, "out", [ port; v ] -> w.out port v; 0
    | None, "inp", [ port ] -> w.inp port land 0xff
    | None, _, _ -> w.ext f args
    | Some g, _, _ ->
      let vars = Hashtbl.create 16 in
      List.iter2 (fun (x, _) v -> Hashtbl.replace vars x v) g.ps args;
      (try block vars g.body; 0 with Ret v -> v)
  and eval vars e =
    let ev = eval vars in
    match e.x with
    | Int n -> n
    | Tf b -> Bool.to_int b
    | Str s -> str s
    | Var v -> Hashtbl.find vars v
    | Glob g -> Hashtbl.find globs g
    | Addr x -> Hashtbl.find addrs x
    | Index _ | Field _ -> assert false
    | Lut (f, a) -> (Hashtbl.find luts f).(ev a land 0xff)
    | Bin ("&&", l, r) -> if ev l <> 0 then ev r else 0
    | Bin ("||", l, r) -> if ev l <> 0 then 1 else ev r
    | Bin (o, l, r) ->
      let a = ev l in
      let b = ev r in
      let cmp c = Bool.to_int c in
      (* 符号付きで比べるときは、符号ビットが立っていれば負の数として見る *)
      let sa, sb =
        if signed l.a then (let s v = if v > mask l.a / 2 then v - mask l.a - 1 else v in s a, s b) else a, b
      in
      (* 符号付きの割り算: 絶対値どうしで割ってから符号を付ける。商は 0 の方へ丸まり、余りは割られる数と同じ符号 *)
      let sdiv () =
        let q, r = if sb = 0 then mask e.a, abs sa else abs sa / abs sb, abs sa mod abs sb in
        (if (sa < 0) <> (sb < 0) then -q else q) land mask e.a, (if sa < 0 then -r else r) land mask e.a
      in
      (match o with
       | "/" when signed l.a -> fst (sdiv ())
       | "%" when signed l.a -> snd (sdiv ())
       | ">>" when signed l.a -> (sa asr b) land mask e.a
       | "+" -> (a + b) land mask e.a
       | "-" -> (a - b) land mask e.a
       | "*" -> (a * b) land mask e.a
       (* 0 で割ると、商は全部のビットが 1、余りは割られる数（機械語のルーチンと同じ） *)
       | "/" -> if b = 0 then mask e.a else a / b
       | "%" -> if b = 0 then a else a mod b
       | "&" -> a land b
       | "|" -> a lor b
       | "^" -> a lxor b
       | "<<" -> (a lsl b) land mask e.a
       | ">>" -> a lsr b
       | "==" -> cmp (a = b)
       | "!=" -> cmp (a <> b)
       | "<" -> cmp (sa < sb)
       | "<=" -> cmp (sa <= sb)
       | ">" -> cmp (sa > sb)
       | ">=" -> cmp (sa >= sb)
       | _ -> assert false)
    | Un ("!", a) -> 1 - ev a
    | Un ("-", a) -> (- (ev a)) land mask e.a
    | Un (_, a) -> lnot (ev a) land mask e.a
    | Call (f, args) -> call f (List.map ev args) land (if e.a = Unit then 0 else mask e.a)
    | Mem (t, a) -> peek t (ev a)
    | Port p -> w.inp (ev p) land 0xff
    | Spawn (t, args) ->
      let vs = List.map (fun (a : ty expr) -> ev a, a.a) args in
      (match pool t with
       | None -> set (t, 0) Fresh; 0
       | Some (n, size) ->
         (* 空いている最初の枠を使う。空きがなければ、走らせない予備の枠（n 番目）を使う。
            引数はフレームの先頭から順に置く。フレームの番地が式の値。 *)
         let i = match List.find_opt (fun i -> state (t, i) = Idle) (slots t) with
           | Some i -> set (t, i) Fresh; i
           | None -> n
         in
         ignore (List.fold_left (fun a (v, ty) -> poke ty a v; a + if wide ty then 2 else 1) (frame t i size) vs);
         frame t i size)
    | Running t -> Bool.to_int (List.exists (fun i -> state (t, i) <> Idle) (slots t))
    | Cast (Bool, a) -> Bool.to_int (ev a <> 0)
    | Cast (t, a) when wide t && a.a = I8 -> let v = ev a in (if v > 127 then v - 256 else v) land mask t
    | Cast (t, a) -> ev a land mask t
  and block vars ss = List.iter (stmt vars) ss
  and stmt vars = function
    | Let (x, _, e) -> Hashtbl.replace vars x (eval vars e)
    | Set ({ x = Var x; _ }, e) -> Hashtbl.replace vars x (eval vars e)
    | Set ({ x = Glob g; _ }, e) -> Hashtbl.replace globs g (eval vars e)
    | Set ({ x = Mem (t, a); _ }, e) -> let a = eval vars a in poke t a (eval vars e)
    | Set ({ x = Port p; _ }, e) -> w.out (eval vars p) (eval vars e)
    | Set _ -> assert false
    | If (c, t, f) -> block vars (if eval vars c <> 0 then t else f)
    | While (c, b) ->
      (try
         while eval vars c <> 0 do
           burn ();
           try block vars b with Cont -> ()
         done
       with Brk -> ())
    | For (x, lo, hi, b) ->
      Hashtbl.replace vars x (eval vars lo);
      let hi = eval vars hi in
      (try
         while Hashtbl.find vars x < hi do
           burn ();
           (try block vars b with Cont -> ());
           Hashtbl.replace vars x ((Hashtbl.find vars x + 1) land mask lo.a)
         done
       with Brk -> ())
    | Match (e, arms, other) ->
      let v = eval vars e in
      let hit (ps, _) = List.exists (fun (p : ty expr) -> eval vars p = v) ps in
      (match List.find_opt hit arms, other with
       | Some (_, b), _ | None, Some b -> block vars b
       | None, None -> ())
    | Break -> raise Brk
    | Continue -> raise Cont
    | Return None -> raise (Ret 0)
    | Return (Some e) -> raise (Ret (eval vars e))
    | Do e -> ignore (eval vars e)
    | Yield -> Effect.perform Pause
    | Wait _ -> assert false
    | Each (x, t, b) -> block vars (Check.each p.pools x t b)
    | Stop (t, None) -> List.iter (fun i -> set (t, i) Idle) (slots t)
    | Stop (t, Some e) ->
      let a = eval vars e and _, size = Option.get (pool t) in
      set (t, (a - frame t 0 size) / size) Idle
    | Resume t ->
      burn ();
      let g = List.find (fun (g : ty fn) -> g.name = t) p.fns in
      List.iter
        (fun i ->
          match state (t, i) with
          | Idle -> ()
          | Paused k -> Effect.Deep.continue k ()
          | Fresh ->
            let vars = Hashtbl.create 16 in
            (match pool t with Some (_, size) -> Hashtbl.replace vars g.self (frame t i size) | None -> ());
            Effect.Deep.match_with
              (fun () -> (try block vars g.body with Ret _ -> ()); set (t, i) Idle)
              ()
              { retc = (fun () -> ());
                exnc = raise;
                effc = (fun (type a) (e : a Effect.t) ->
                  match e with
                  | Pause -> Some (fun (k : (a, unit) Effect.Deep.continuation) -> set (t, i) (Paused k))
                  | _ -> None) })
        (slots t)
  in
  call

let run p w = ignore (load p w "main" [])
