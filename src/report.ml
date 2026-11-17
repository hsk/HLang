(* コンパイラのレポートに載せる「時間の上限」。

   出したコードを、命令を点、次に実行されうる命令への矢印を辺とするグラフとして見て、
   入口から ret までのいちばん長い道の T ステートを求める。輪（ループ）に入る道があれば、
   上限は分からない。ただし ld b,n で始まる djnz のループは n 回と分かるので、数えられる。タスクは yield のたびに ret するので、「待ちながら回るループ」は輪にならず、
   「再開してから次の中断まで」の上限が出る。 *)

open Asm

type bound =
  | Known of int * bool          (* T ステートの上限, 外の関数（BIOS など）を呼ぶか。呼ぶなら、その時間は含まない *)
  | Unknown

exception Loop

(* いちばん長い道の T ステート。上限が分からなければ Loop。
   code の終わりから落ちる道は、そこで終わりとみなす（ループの本体だけを測るのに使う）。 *)
let rec longest code tables (callee : string -> bound) ext starts =
  let at = Hashtbl.create 32 in
  Array.iteri (fun i l -> match l with Label s -> Hashtbl.replace at s i | _ -> ()) code;
  let rec syms acc = function Sym s -> s :: acc | Add (a, b) -> syms (syms acc a) b | Lo a | Hi a | Uop (_, a) -> syms acc a | Bop (_, a, b) -> syms (syms acc a) b | Num _ | Here -> acc in
  let targets = List.concat_map (function Db es -> List.fold_left syms [] es | _ -> []) tables in
  let memo = Array.make (Array.length code + 1) (-1) and busy = Array.make (Array.length code + 1) false in
  let rec go i =
    if i >= Array.length code then 0
    else if busy.(i) then raise Loop
    else if memo.(i) >= 0 then memo.(i)
    else begin
      busy.(i) <- true;
      let next () = go (i + 1) in
      (* 同じコードの中のラベルならそこから、外なら呼ぶ先の上限 *)
      let call f =
        match Hashtbl.find_opt at f with
        | Some j -> go j
        | None -> (match callee f with Known (t, e) -> if e then ext := true; t | Unknown -> raise Loop)
      in
      (* 回数の決まったループ: ラベルの直前が ld b,n（か ld bc,n）なら、djnz は n 回まわる *)
      let rec count j =
        if j < 0 then None
        else match code.(j) with
          | Label _ | Rem _ -> count (j - 1)
          | Ins ("ld", [ R "b"; I (Num n) ]) -> Some (if n land 255 = 0 then 256 else n land 255)
          | Ins ("ld", [ R "bc"; I (Num n) ]) -> Some (if n lsr 8 land 255 = 0 then 256 else n lsr 8 land 255)
          | _ -> None
      in
      let r =
        match code.(i) with
        | Ins ((op, args) as ins) ->
          let t = snd (fst (Cost.measure ins)) in
          t
          + (match op, args with
             | "ret", [] -> 0
             | "ret", _ -> next ()
             | ("jp" | "jr"), [ I (Sym l) ] -> call l                       (* 関数の外へ飛ぶのは末尾呼び出し *)
             | ("jp" | "jr"), [ R _; I (Sym l) ] -> max (call l) (next ())
             | "djnz", [ I (Sym l) ] ->
               (match Hashtbl.find_opt at l with
                | Some j when j < i ->
                  (match count (j - 1) with
                   | Some n ->
                     (* 1 回分はここへ来る道でもう数えてある。残りの n - 1 回分を足す。
                        本体から外へ出る道は、外側のコードで測る *)
                     let outer f = match Hashtbl.find_opt at f with Some k -> Known (go k, false) | None -> callee f in
                     let body = longest (Array.sub code j (i - j)) tables outer ext [ l ] in
                     ((n - 1) * (body + t)) + next ()
                   | None -> raise Loop)
                | _ -> max (call l) (next ()))
             | "jp", [ M (R "hl") ] ->
               if targets = [] then raise Loop else List.fold_left (fun m l -> max m (call l)) 0 targets
             | "jp", [ I _ ] -> ext := true; 0
             | "call", [ I (Sym f) ] -> let c = call f in c + next ()
             | "call", [ R _; I (Sym f) ] -> let c = call f in c + next ()
             | "call", _ -> ext := true; next ()
             | _ -> next ())
        | _ -> next ()
      in
      busy.(i) <- false;
      memo.(i) <- r;
      r
    end
  in
  List.fold_left (fun m s -> max m (go (Hashtbl.find at s))) 0 starts

(* lines: 関数 1 つのコード。tables: その関数の飛び先の表。callee: 呼ぶ先の上限。starts: 入口のラベル。 *)
let bound lines tables callee starts =
  let ext = ref false in
  try
    let t = longest (Array.of_list lines) tables callee ext starts in
    Known (t, !ext)
  with Loop | Not_found -> Unknown
