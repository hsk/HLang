(* 置き場所の割り当て。変数をどのレジスタ（またはメモリ）に置くかを、実際にコードを出して
   測りながら決める。関数の引数の置き場所も同じ仕組みで決まるので、呼び出し規約は関数ごとに変わる。

   進め方:
   0. 全部メモリに置く。これは必ずコードが出せる。
   1. 受け渡しのある変数どうし（継続の引数と渡す値、演算の結果と元の値）を同じ置き場所にまとめてみる。
      コストが悪くならなければ採用。受け渡しの命令が消える。
   2. まとまりごとに、各レジスタへ置いてみる。いちばん安くなる置き場所を採用。重い変数から順に。
   コードが出せない割り当て（Gen.Stuck）は単に不採用になる。 *)

open Ir
open Asm
module L = Live

(* ---- 仕上げの置き換え ---- *)

let neg = function "z" -> "nz" | "nz" -> "z" | "c" -> "nc" | "nc" -> "c" | c -> c

(* 行には (行, 重み) の形で、何回実行されそうかの重みが付いている *)

let rec next_label l = function
  | (Label l', _) :: rest -> l = l' || next_label l rest
  | (Rem _, _) :: rest -> next_label l rest
  | _ -> false

let rec first = function                     (* ラベルやコメントを飛ばした最初の命令 *)
  | ((Label _ | Rem _), _) :: rest -> first rest
  | (Ins i, _) :: _ -> Some i
  | _ -> None

let rec refs_e acc = function
  | Sym s -> s :: acc
  | Add (x, y) -> refs_e (refs_e acc x) y
  | Lo x | Hi x -> refs_e acc x
  | Num _ -> acc

let rec refs_arg acc = function I e | X (_, e) -> refs_e acc e | M x -> refs_arg acc x | R _ -> acc

let refs code =
  List.fold_left (fun acc (l, _) -> match l with Ins (_, args) -> List.fold_left refs_arg acc args | _ -> acc) [] code

(* 飛び先がすぐ ret や別のジャンプなら、そこを経由しない *)
let thread code =
  let at = Hashtbl.create 16 in
  let rec scan = function
    | (Label l, _) :: rest -> Hashtbl.replace at l (first rest); scan rest
    | _ :: rest -> scan rest
    | [] -> ()
  in
  scan code;
  let target l = Option.join (Hashtbl.find_opt at l) in
  List.map
    (fun (line, d) ->
      ( (match line with
         | Ins ("jp", [ I (Sym l) ]) ->
           (match target l with
            | Some ("ret", []) -> Ins ("ret", [])
            | Some ("jp", [ I t ]) -> Ins ("jp", [ I t ])
            | _ -> line)
         | Ins ("jp", [ R cc; I (Sym l) ]) ->
           (match target l with
            | Some ("ret", []) -> Ins ("ret", [ R cc ])
            | Some ("jp", [ I t ]) -> Ins ("jp", [ R cc; I t ])
            | _ -> line)
         | _ -> line),
        d ))
    code

let rec rewrite = function
  (* すぐ次の行へのジャンプは要らない *)
  | (Ins ("jp", [ I (Sym l) ]), _) :: rest when next_label l rest -> rewrite rest
  (* 条件付きで 1 つ飛び越すジャンプは、条件を逆にして 1 つにする *)
  | (Ins ("jp", [ R cc; I (Sym l) ]), d) :: (Ins ("jp", [ I t ]), _) :: rest when next_label l rest ->
    rewrite ((Ins ("jp", [ R (neg cc); I t ]), d) :: rest)
  | (Ins ("jp", [ R cc; I (Sym l) ]), d) :: (Ins ("ret", []), _) :: rest when next_label l rest ->
    rewrite ((Ins ("ret", [ R (neg cc) ]), d) :: rest)
  (* 呼んですぐ戻るなら、飛ぶだけでよい *)
  | (Ins ("call", x), d) :: (Ins ("ret", []), _) :: rest -> (Ins ("jp", x), d) :: rewrite rest
  | x :: rest -> x :: rewrite rest
  | [] -> []

(* 無条件に飛んだ後、誰も飛んでこない所までは実行されない *)
let dead keep code =
  let live = Hashtbl.create 32 in
  List.iter (fun l -> Hashtbl.replace live l ()) (keep @ refs code);
  let rec go skip = function
    | [] -> []
    | (Rem _, _) :: ((Label l, _) :: _ as rest) when skip && not (Hashtbl.mem live l) -> go true rest
    | ((Rem _, _) as r) :: ((Label _, _) :: _ as rest) -> r :: go false rest
    | ((Label l, _) as x) :: rest -> if skip && not (Hashtbl.mem live l) then go true rest else x :: go false rest
    | x :: rest when skip -> ignore x; go true rest
    | ((Ins (("jp" | "ret"), ([] | [ I _ ] | [ M _ ])), _) as x) :: rest -> x :: go true rest
    | x :: rest -> x :: go false rest
  in
  go false code

(* 1 回の置き換えが次の置き換えを可能にすることがあるので、2 回かける *)
let tidy keep code =
  let once code = dead keep (rewrite (thread code)) in
  Cost.short (once (once code))

(* ---- 関数 1 つの割り当て ---- *)

let trials = ref 0 and stuck = ref 0             (* 試した回数と、行き詰まった回数 *)
(* 関数 1 つで試す回数の上限。1 回の手間は関数の大きさに比例するので、大きい関数ほど回数を減らす。 *)
let budget nvars = max 200 (min 1500 (150_000 / max 1 nvars))
let regs8 = [ "a"; "b"; "c"; "d"; "e"; "h"; "l" ]
let regs16 = [ "hl"; "de"; "bc" ]

(* 割り当てを探す。戻り値は (コード, 表, 置き場所, 候補の番地)。 *)
let solve ~opt ~info ~exts (f : Ir.fn) body vars =
  let cls = Hashtbl.create 64 in                       (* 変数 -> まとまりの代表 *)
  let chome = Hashtbl.create 64 in                     (* 代表 -> 置き場所。"" はメモリ *)
  List.iter (fun (x, _, _) -> Hashtbl.replace cls x x; Hashtbl.replace chome x "") vars;
  let home x =
    let r = Hashtbl.find cls x in
    match Hashtbl.find chome r with "" -> "@" ^ f.name ^ "." ^ r | h -> h
  in
  let gen () =
    let code, data = Gen.fn ~info ~exts ~home ~pick:opt f body in
    (* 表から参照される飛び先は、消してはいけない *)
    let keep = List.concat_map (function Db es -> List.fold_left refs_e [] es | _ -> []) data in
    tidy (f.name :: (f.name ^ ".done") :: keep) code, data
  in
  let price (code, data) = Cost.price code data in
  (* 試せる回数には上限を付ける。大きな関数で時間が膨らまないように。
     重い変数から順に試すので、上限に達しても大事な変数は済んでいる。 *)
  let left = ref (budget (List.length vars)) in
  let try_ () =
    incr trials;
    decr left;
    match gen () with
    | r -> Some (price r)
    | exception Gen.Stuck why ->
      incr stuck;
      (* HL_WHY=変数名 を付けると、その変数の置き場所を試して行き詰まった理由を出す *)
      (match Sys.getenv_opt "HL_WHY" with
       | Some v when Hashtbl.mem cls v && not (Gen.is_mem (home v)) ->
         Printf.eprintf "%s を %s に置けない: %s\n" v (home v) why
       | _ -> ());
      None
  in
  let best = ref (price (gen ())) in
  if opt then begin
    let ty x = let _, t, _ = List.find (fun (y, _, _) -> y = x) vars in t in
    List.iter
      (fun (x, y) ->
        let cx = Hashtbl.find cls x and cy = Hashtbl.find cls y in
        if !left > 0 && cx <> cy && ty x = ty y then begin
          let moved = List.filter (fun (v, _, _) -> Hashtbl.find cls v = cy) vars in
          let set c = List.iter (fun (v, _, _) -> Hashtbl.replace cls v c) moved in
          set cx;
          match try_ () with Some c when c <= !best -> best := c | _ -> set cy
        end)
      (L.pairs f);
    (* まとまりの重さ（中の変数の重さの合計）の順に並べる。重いものから良い場所を取る。 *)
    let weight = Hashtbl.create 64 in
    List.iter
      (fun (x, _, n) ->
        let r = Hashtbl.find cls x in
        Hashtbl.replace weight r (n + Option.value ~default:0 (Hashtbl.find_opt weight r)))
      vars;
    let reps =
      List.stable_sort
        (fun (x, _, _) (y, _, _) -> compare (Hashtbl.find weight y) (Hashtbl.find weight x))
        (List.filter (fun (x, _, _) -> Hashtbl.find cls x = x) vars)
    in
    (* 2 周する。周りの置き場所が決まった後で、もっと良い場所が見つかることがある。 *)
    for _ = 1 to 2 do
      List.iter
        (fun (x, t, _) ->
          if Hashtbl.find cls x = x then begin
            let cur = Hashtbl.find chome x in
            let pick = ref cur in
            List.iter
              (fun h ->
                if h <> cur && !left > 0 then begin
                  Hashtbl.replace chome x h;
                  match try_ () with Some c when c < !best -> best := c; pick := h | _ -> ()
                end)
              ((if t = U8 then regs8
                else if Option.map (Hashtbl.find cls) f.self = Some x then [ "hl"; "ix"; "de"; "bc" ]
                else regs16)
               @ [ "" ]);
            Hashtbl.replace chome x !pick
          end)
        reps
    done
  end;
  let code, data = gen () in
  (* メモリ上の置き場所の候補。実際に使われたものだけ、後で番地を割り当てる。 *)
  let slots =
    List.filter_map
      (fun (x, t, _) ->
        let h = home x in
        if Gen.is_mem h && Hashtbl.find cls x = x then Some (String.sub h 1 (String.length h - 1), t) else None)
      vars
  in
  code, data, List.map (fun (p, _) -> home p) f.ps, slots

(* プールのタスクを動かすルーチン。n 個のフレームが size バイトおきに並ぶ。
   フレームは「続きの番地 2 バイト、変数」。番地の上位が 0 なら空き。 *)
let pool_code t n size =
  let l s = t ^ "." ^ s and i n = I (Num n) and hl = R "hl" and de = R "de" and a = R "a" in
  let ins op args = Ins (op, args) in
  let scan = [ ins "ld" [ hl; I (Add (Sym (l "pool"), Num 1)) ]; ins "ld" [ de; i size ] ] in
  [ l "resume", [                             (* resume: 全部の個体を、次の yield まで進める *)
      Label (l "resume"); ins "ld" [ hl; I (Sym (l "pool")) ]; ins "ld" [ R "b"; i n ];
      Label (l "each"); ins "push" [ R "bc" ]; ins "push" [ hl ]; ins "call" [ I (Sym (l "go")) ];
      ins "pop" [ hl ]; ins "pop" [ R "bc" ]; ins "ld" [ de; i size ]; ins "add" [ hl; de ];
      ins "djnz" [ I (Sym (l "each")) ]; ins "ret" [];
      Label (l "go");                         (* 続きの番地を読み、HL をフレームの変数に向けて、そこへ飛ぶ *)
      ins "ld" [ R "e"; M hl ]; ins "inc" [ hl ]; ins "ld" [ R "d"; M hl ]; ins "inc" [ hl ];
      ins "ld" [ a; R "d" ]; ins "or" [ a ]; ins "ret" [ R "z" ]; ins "push" [ de ]; ins "ret" [] ];
    l "spawn", [                              (* spawn: 空きを探して最初から始める状態にし、HL で変数の場所を返す *)
      Label (l "spawn") ] @ scan @ [ ins "ld" [ R "b"; i n ];
      Label (l "find"); ins "ld" [ a; M hl ]; ins "or" [ a ]; ins "jr" [ R "z"; I (Sym (l "take")) ];
      ins "add" [ hl; de ]; ins "djnz" [ I (Sym (l "find")) ];
      ins "inc" [ hl ]; ins "ret" [];         (* 空きがない: 走らせない予備の枠を返す。引数はそこへ捨てられる *)
      Label (l "take"); ins "ld" [ M hl; I (Hi (Sym t)) ]; ins "dec" [ hl ]; ins "ld" [ M hl; I (Lo (Sym t)) ];
      ins "inc" [ hl ]; ins "inc" [ hl ]; ins "ret" [] ];
    l "live", [                               (* running: 動いている個体の数を A で返す *)
      Label (l "live") ] @ scan @ [ ins "ld" [ R "bc"; i (n * 256) ];
      Label (l "count"); ins "ld" [ a; M hl ]; ins "or" [ a ]; ins "jr" [ R "z"; I (Sym (l "skip")) ]; ins "inc" [ R "c" ];
      Label (l "skip"); ins "add" [ hl; de ]; ins "djnz" [ I (Sym (l "count")) ]; ins "ld" [ a; R "c" ]; ins "ret" [] ] ]

(* 割り算と余りのルーチン: (名前, 引数の置き場所, 壊すレジスタ, コード)。使われたものだけ ROM に入る。
   1 ビットずつ、余りを左へ送りながら引けるか試す。0 で割ると商は全ビット 1、余りは割られる数。 *)
let divmod =
  let ins op args = Ins (op, args) and r x = R x and to_ l = I (Sym l) in
  let hl = r "hl" and de = r "de" and a = r "a" in
  [ ".mod8", [ "a"; "e" ], [ "a"; "b"; "d" ], [               (* A / E。余りが A、商が D *)
      Label ".mod8"; ins "ld" [ r "d"; a ]; ins "xor" [ a ]; ins "ld" [ r "b"; I (Num 8) ];
      Label ".mod8.bit"; ins "sla" [ r "d" ]; ins "rla" []; ins "cp" [ r "e" ]; ins "jr" [ r "c"; to_ ".mod8.next" ];
      ins "sub" [ r "e" ]; ins "inc" [ r "d" ];
      Label ".mod8.next"; ins "djnz" [ to_ ".mod8.bit" ]; ins "ret" [] ];
    ".div8", [ "a"; "e" ], [ "a"; "b"; "d" ], [
      Label ".div8"; ins "call" [ to_ ".mod8" ]; ins "ld" [ a; r "d" ]; ins "ret" [] ];
    ".mod16", [ "hl"; "de" ], [ "a"; "b"; "c"; "h"; "l" ], [    (* HL / DE。余りが HL、商が AC *)
      Label ".mod16"; ins "ld" [ a; r "h" ]; ins "ld" [ r "c"; r "l" ]; ins "ld" [ hl; I (Num 0) ];
      ins "ld" [ r "b"; I (Num 16) ];
      Label ".mod16.bit"; ins "sla" [ r "c" ]; ins "rla" []; ins "adc" [ hl; hl ]; ins "jr" [ r "c"; to_ ".mod16.over" ];
      ins "sbc" [ hl; de ]; ins "jr" [ r "nc"; to_ ".mod16.fits" ]; ins "add" [ hl; de ]; ins "jr" [ to_ ".mod16.next" ];
      Label ".mod16.over"; ins "or" [ a ]; ins "sbc" [ hl; de ];
      Label ".mod16.fits"; ins "inc" [ r "c" ];
      Label ".mod16.next"; ins "djnz" [ to_ ".mod16.bit" ]; ins "ret" [] ];
    ".div16", [ "hl"; "de" ], [ "a"; "b"; "c"; "h"; "l" ], [
      Label ".div16"; ins "call" [ to_ ".mod16" ]; ins "ld" [ r "h"; a ]; ins "ld" [ r "l"; r "c" ]; ins "ret" [] ];
    (* 符号付き: 絶対値どうしで割ってから符号を付ける。商の符号は 2 つの符号の xor、余りの符号は割られる数と同じ *)
    ".sdiv8", [ "a"; "e" ], [ "a"; "b"; "c"; "d"; "e" ], [
      Label ".sdiv8"; ins "ld" [ r "d"; a ]; ins "xor" [ r "e" ]; ins "ld" [ r "c"; a ]; ins "ld" [ a; r "d" ];
      ins "call" [ to_ ".abs8" ]; ins "call" [ to_ ".mod8" ]; ins "ld" [ a; r "d" ];
      ins "bit" [ I (Num 7); r "c" ]; ins "ret" [ r "z" ]; ins "neg" []; ins "ret" [] ];
    ".smod8", [ "a"; "e" ], [ "a"; "b"; "c"; "d"; "e" ], [
      Label ".smod8"; ins "ld" [ r "c"; a ]; ins "call" [ to_ ".abs8" ]; ins "call" [ to_ ".mod8" ];
      ins "bit" [ I (Num 7); r "c" ]; ins "ret" [ r "z" ]; ins "neg" []; ins "ret" [] ];
    ".abs8", [], [ "a"; "d"; "e" ], [                           (* A と E を絶対値にする *)
      Label ".abs8"; ins "or" [ a ]; ins "jp" [ r "p"; to_ ".abs8.e" ]; ins "neg" [];
      Label ".abs8.e"; ins "ld" [ r "d"; a ]; ins "ld" [ a; r "e" ]; ins "or" [ a ]; ins "jp" [ r "p"; to_ ".abs8.ok" ];
      ins "neg" []; ins "ld" [ r "e"; a ];
      Label ".abs8.ok"; ins "ld" [ a; r "d" ]; ins "ret" [] ];
    ".sdiv16", [ "hl"; "de" ], all8, [
      Label ".sdiv16"; ins "ld" [ a; r "h" ]; ins "xor" [ r "d" ]; ins "push" [ r "af" ];
      ins "call" [ to_ ".abs16" ]; ins "call" [ to_ ".mod16" ]; ins "ld" [ r "h"; a ]; ins "ld" [ r "l"; r "c" ];
      ins "pop" [ r "af" ]; ins "ret" [ r "p" ]; ins "jp" [ to_ ".neg16" ] ];
    ".smod16", [ "hl"; "de" ], all8, [
      Label ".smod16"; ins "ld" [ a; r "h" ]; ins "or" [ a ]; ins "push" [ r "af" ];
      ins "call" [ to_ ".abs16" ]; ins "call" [ to_ ".mod16" ];
      ins "pop" [ r "af" ]; ins "ret" [ r "p" ]; ins "jp" [ to_ ".neg16" ] ];
    ".abs16", [], all8, [                                       (* HL と DE を絶対値にする *)
      Label ".abs16"; ins "bit" [ I (Num 7); r "h" ]; ins "call" [ r "nz"; to_ ".neg16" ];
      ins "bit" [ I (Num 7); r "d" ]; ins "ret" [ r "z" ];
      ins "xor" [ a ]; ins "sub" [ r "e" ]; ins "ld" [ r "e"; a ]; ins "sbc" [ a; a ]; ins "sub" [ r "d" ];
      ins "ld" [ r "d"; a ]; ins "ret" [] ];
    ".neg16", [], [ "a"; "h"; "l" ], [                          (* HL の符号を変える *)
      Label ".neg16"; ins "xor" [ a ]; ins "sub" [ r "l" ]; ins "ld" [ r "l"; a ]; ins "sbc" [ a; a ]; ins "sub" [ r "h" ];
      ins "ld" [ r "h"; a ]; ins "ret" [] ] ]

let fn ~opt ~info ~exts (f : Ir.fn) =
  let code, data, phome, slots = solve ~opt ~info ~exts f (L.annotate f) (L.vars f) in
  (* 壊すレジスタ: 自分が書くものと、呼んだ先が壊すもの *)
  let clob =
    List.sort_uniq compare
      (List.concat_map
         (fun (line, _) ->
           match line with
           | Ins (("call" | "jp"), [ I (Sym g) ]) when Hashtbl.mem info g -> (Hashtbl.find info g : Gen.info).clob
           | Ins (("call" | "jp"), [ I (Num n) ]) ->
             (match List.find_opt (fun (_, (_, _, addr, _)) -> addr = n) exts with
              | Some (_, (_, _, _, clob)) -> clob
              | None -> all8)
           | Ins ("call", _) -> all8
           | Ins i -> writes i
           | _ -> [])
         code)
  in
  if not f.task || f.self <> None then List.map fst code, data, { Gen.phome; clob }, slots
  else begin
    (* タスク: 続きの番地を置く場所と、そこへ飛ぶ入口を足す *)
    let pc = f.name ^ ".pc" in
    ( List.map fst code
      @ [ Label (f.name ^ ".resume"); Ins ("ld", [ R "hl"; M (I (Sym pc)) ]); Ins ("jp", [ M (R "hl") ]) ],
      data, { Gen.phome; clob }, slots @ [ pc, U16 ] )
  end

(* ---- プログラム全体 ---- *)

(* レポート用: 出した関数ごとの (関数, コードのバイト数, 引数の置き場所, 壊すレジスタ, 時間の上限) と、RAM の割り当て *)
type row = { fn : Ir.fn; bytes : int; homes : string list; clobbers : string list; time : Report.bound }
let report : row list ref = ref []
let ram : (string * int) list ref = ref []

type target = {
  org : int;                     (* プログラムを置く番地 *)
  ram : int;                     (* 変数を置く番地 *)
  head : line list;              (* 先頭に置くもの（ROM ヘッダなど） *)
}

(* 項が名前で指しているもの: 呼ぶ関数と、番地を使う関数・タスク *)
let rec uses t =
  let syms l = List.filter_map (function S s -> Some s | _ -> None) l in
  match t with
  | Ir.Call (_, g, args, k) -> (g :: syms (List.map fst args)) @ uses k
  | Ir.Let (_, _, e, k) -> syms (L.atoms_of e) @ uses k
  | Ir.Store (_, a, v, k) -> syms [ a; v ] @ uses k
  | Ir.Out (_, v, k) -> syms [ v ] @ uses k
  | Ir.Yield k -> uses k
  | Ir.LetK (ks, b) -> uses b @ List.concat_map (fun (k : Ir.kont) -> uses k.body) ks
  | Ir.If (_, _, a, b, x, y) -> syms [ a; b ] @ uses x @ uses y
  | Ir.Jump (_, args) -> syms args
  | Ir.Switch (a, _, _) -> syms [ a ]
  | Ir.Ret r -> syms (List.map fst (Option.to_list r))

let program ~opt (tg : target) (p : Check.prog) (fns : Ir.fn list) : line list =
  let info = Hashtbl.create 16 in
  let code = ref [] and slots = ref [] and tables = ref [] in
  (* 呼ばれる側から先に決める。呼ぶ側は、相手の引数の置き場所と壊すレジスタを見て決められる。 *)
  (* main からたどれる関数だけを出す。使わないライブラリの関数は ROM に入らない。 *)
  let find g =
    List.find_opt (fun (h : Ir.fn) -> List.exists (fun s -> h.name ^ s = g) [ ""; ".resume"; ".spawn"; ".live"; ".pool"; ".pc"; ".done" ]) fns
  in
  List.iter (fun (l, phome, clob, _) -> Hashtbl.replace info l { Gen.phome; clob }) divmod;
  let times = Hashtbl.create 16 in
  report := [];
  let tasks = ref [] and seen = Hashtbl.create 16 and extra = ref [] and frames = ref [] in
  (* 呼ぶ先の時間の上限。割り算のルーチンは、初めて聞かれたときに測る *)
  let rec callee g =
    match Hashtbl.find_opt times g, List.find_opt (fun (l, _, _, _) -> l = g) divmod with
    | Some b, _ -> b
    | None, Some (_, _, _, lines) ->
      let b = Report.bound lines [] callee [ g ] in
      Hashtbl.replace times g b;
      b
    | None, None -> Report.Unknown
  in
  let rec visit (f : Ir.fn) =
    if not (Hashtbl.mem seen f.name) then begin
      Hashtbl.replace seen f.name ();
      if f.task then begin
        (* resume は何を壊すか分からない。本体より先に登録しておく（本体から自分を spawn できる） *)
        let all = { Gen.phome = []; clob = "ix" :: all8 } in
        List.iter (fun s -> Hashtbl.replace info (f.name ^ s) all) [ ""; ".resume"; ".spawn"; ".live" ];
        match List.find_opt (fun (t, _, _) -> t = f.name) p.pools with
        | Some (t, n, size) ->
          extra := !extra @ pool_code t n size;
          frames := (t ^ ".pool", (n + 1) * size) :: !frames        (* 最後の 1 つは予備の枠 *)
        | None -> tasks := f.name :: !tasks
      end;
      List.iter (fun g -> Option.iter visit (find g)) (uses f.body);
      let c, d, i, s = fn ~opt ~info ~exts:p.exts f in
      Hashtbl.replace info f.name i;
      (* タスクは、入口と、中断から再開する場所のそれぞれから測る *)
      let starts =
        f.name :: List.filter_map (function Label l when f.task && String.starts_with ~prefix:(f.name ^ ".resume") l
                                                         && l <> f.name ^ ".resume" -> Some l | _ -> None) c
      in
      let time = Report.bound c d callee starts in
      Hashtbl.replace times f.name time;
      (* resume の時間: 1 個だけのタスクは「続きへ飛ぶ」分を足す。プールは、個体を順に回る手間と、個体の数 × 本体 *)
      let plus k = function
        | Report.Known (t, e), Report.Known (u, e') -> Report.Known (t + (k * u), e || e')
        | _ -> Report.Unknown
      in
      if f.task then begin
        match List.find_opt (fun (t, _, _) -> t = f.name) p.pools with
        | Some (t, n, size) ->
          List.iter
            (fun (l, lines) ->
              let b = Report.bound lines [] callee [ l ] in
              Hashtbl.replace times l (if l = t ^ ".resume" then plus n (b, time) else b))
            (pool_code t n size)
        | None ->
          let t i = snd (fst (Cost.measure i)) in
          let jump = t ("ld", [ R "hl"; M (I (Num 0)) ]) + t ("jp", [ M (R "hl") ]) in
          Hashtbl.replace times (f.name ^ ".resume") (plus 1 (Report.Known (jump, false), time))
      end;
      let bytes = List.fold_left (fun n l -> match l with Ins x -> n + snd (Cost.measure x) | _ -> n) 0 c in
      report := !report @ [ { fn = f; bytes; homes = i.phome; clobbers = i.clob; time } ];
      code := !code @ c;
      tables := !tables @ d;
      slots := !slots @ s
    end
  in
  visit (Option.get (find "main"));
  (* コードに名前が出てくる置き場所だけ残す *)
  let used = Hashtbl.create 64 in
  let rec syms = function
    | Sym s -> Hashtbl.replace used s ()
    | Add (x, y) -> syms x; syms y
    | Lo x | Hi x -> syms x
    | Num _ -> ()
  in
  let rec arg = function I e -> syms e | M x -> arg x | X (_, e) -> syms e | R _ -> () in
  List.iter (function Ins (_, args) -> List.iter arg args | _ -> ()) !code;
  (* プールのルーチンは、呼ばれているものだけ足す *)
  code := !code @ List.concat_map (fun (l, lines) -> if Hashtbl.mem used l then lines else []) !extra;
  (* 割り算のルーチン。使われたものと、それが呼ぶもの（div は mod を呼ぶ）だけ足す *)
  let rec grow () =
    let n = Hashtbl.length used in
    List.iter
      (fun (l, _, _, lines) ->
        if Hashtbl.mem used l then List.iter (function Ins (_, args) -> List.iter arg args | _ -> ()) lines)
      divmod;
    if Hashtbl.length used > n then grow ()
  in
  grow ();
  code := !code @ List.concat_map (fun (l, _, _, lines) -> if Hashtbl.mem used l then lines else []) divmod;
  slots := List.filter (fun (l, _) -> Hashtbl.mem used l) !slots;
  (* RAM に置くもの: (名前, バイト数)。大域変数、関数の変数、配列の順。 *)
  let w t = if Ast.wide t then 2 else 1 in
  let vars =
    List.map (fun (x, t, _) -> x, w t) p.globals
    @ List.map (fun (x, t) -> x, if t = U8 then 1 else 2) !slots
    @ List.map (fun (x, t, n) -> x, n * w t) p.arrays
    @ List.rev !frames
  in
  (* 256 バイト以下の配列は、ページをまたがない番地に置く *)
  let equs, last =
    List.fold_left
      (fun (l, at) (x, k) ->
        let at = if k > 2 && k <= 256 && (at land 0xff) + k > 256 then (at lor 0xff) + 1 else at in
        Equ (x, Num at) :: l, at + k)
      ([], tg.ram) vars
  in
  let size = last - tg.ram in
  ram := vars;
  let hl = R "hl" and num n = I (Num n) in
  let clear =
    if size = 0 then []
    else if size = 1 then [ Ins ("xor", [ R "a" ]); Ins ("ld", [ M (num tg.ram); R "a" ]) ]
    else [
      Ins ("ld", [ hl; num tg.ram ]); Ins ("ld", [ R "de"; num (tg.ram + 1) ]);
      Ins ("ld", [ R "bc"; num (size - 1) ]); Ins ("ld", [ M hl; num 0 ]); Ins ("ldir", []);
    ]
  in
  let init =
    List.concat_map
      (fun (x, t, v) ->
        if v = 0 then []
        else if Ast.wide t then [ Ins ("ld", [ hl; num v ]); Ins ("ld", [ M (I (Sym x)); hl ]) ]
        else [ Ins ("ld", [ R "a"; num v ]); Ins ("ld", [ M (I (Sym x)); R "a" ]) ])
      p.globals
    @ List.concat_map                                 (* タスクは「終わった」状態から始まる *)
        (fun t -> [ Ins ("ld", [ hl; I (Sym (t ^ ".done")) ]); Ins ("ld", [ M (I (Sym (t ^ ".pc"))); hl ]) ])
        !tasks
  in
  let strs =
    Hashtbl.fold
      (fun s l acc ->
        Label l :: Db (List.map (fun ch -> Num (Char.code ch)) (List.of_seq (String.to_seq s)) @ [ Num 0 ]) :: acc)
      Cps.strs []
  in
  let datas =
    List.concat_map
      (fun (x, t, vs) ->
        let es = List.map (fun v -> Num v) vs in
        [ Fit (List.length vs * w t); Label x; (if Ast.wide t then Dw es else Db es) ])
      p.datas
  in
  (* 表にした関数: 256 バイト境界に下位 256 個、16bit ならすぐ次のページに上位 256 個 *)
  let luts =
    List.concat_map
      (fun (f, t, vs) ->
        [ Align 256; Label (f ^ ".lut"); Db (List.map (fun v -> Num (v land 0xff)) vs) ]
        @ if Ast.wide t then [ Db (List.map (fun v -> Num (v lsr 8)) vs) ] else [])
      p.luts
  in
  tg.head
  @ [ Label "start" ] @ clear @ init
  @ [ Ins ("call", [ I (Sym "main") ]); Ins ("di", []); Ins ("halt", []) ]
  @ !code @ strs @ datas @ !tables @ luts @ List.rev equs
