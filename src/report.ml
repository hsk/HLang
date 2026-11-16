(* コンパイラのレポートに載せる「時間の上限」。

   出したコードを、命令を点、次に実行されうる命令への矢印を辺とするグラフとして見て、
   入口から ret までのいちばん長い道の T ステートを求める。輪（ループ）に入る道があれば、
   上限は分からない。タスクは yield のたびに ret するので、「待ちながら回るループ」は輪にならず、
   「再開してから次の中断まで」の上限が出る。 *)

open Asm

type bound =
  | Known of int * bool          (* T ステートの上限, 外の関数（BIOS など）を呼ぶか。呼ぶなら、その時間は含まない *)
  | Unknown

exception Loop

(* lines: 関数 1 つのコード。tables: その関数の飛び先の表。callee: 呼ぶ先の上限。starts: 入口のラベル。 *)
let bound lines tables (callee : string -> bound) starts =
  let code = Array.of_list lines in
  let at = Hashtbl.create 32 in
  Array.iteri (fun i l -> match l with Label s -> Hashtbl.replace at s i | _ -> ()) code;
  let rec syms acc = function Sym s -> s :: acc | Add (a, b) -> syms (syms acc a) b | Lo a | Hi a -> syms acc a | Num _ -> acc in
  let targets = List.concat_map (function Db es -> List.fold_left syms [] es | _ -> []) tables in
  let ext = ref false in
  let memo = Array.make (Array.length code + 1) (-1) and busy = Array.make (Array.length code + 1) false in
  let call f = match callee f with Known (t, e) -> if e then ext := true; t | Unknown -> raise Loop in
  let rec go i =
    if i >= Array.length code then 0
    else if busy.(i) then raise Loop
    else if memo.(i) >= 0 then memo.(i)
    else begin
      busy.(i) <- true;
      let next () = go (i + 1) in
      let jump l = match Hashtbl.find_opt at l with Some j -> go j | None -> call l in     (* 関数の外へ飛ぶのは末尾呼び出し *)
      let r =
        match code.(i) with
        | Ins ((op, args) as ins) ->
          let t = snd (fst (Cost.measure ins)) in
          t
          + (match op, args with
             | "ret", [] -> 0
             | "ret", _ -> next ()
             | ("jp" | "jr"), [ I (Sym l) ] -> jump l
             | ("jp" | "jr"), [ R _; I (Sym l) ] | "djnz", [ I (Sym l) ] -> max (jump l) (next ())
             | "jp", [ M (R "hl") ] ->
               if targets = [] then raise Loop else List.fold_left (fun m l -> max m (jump l)) 0 targets
             | "jp", [ I _ ] -> ext := true; 0
             | "call", [ I (Sym f) ] -> let c = call f in c + next ()
             | "call", _ -> ext := true; next ()
             | _ -> next ())
        | _ -> next ()
      in
      busy.(i) <- false;
      memo.(i) <- r;
      r
    end
  in
  try
    let t = List.fold_left (fun m s -> max m (go (Hashtbl.find at s))) 0 starts in
    Known (t, !ext)
  with Loop | Not_found -> Unknown
