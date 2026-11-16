(* 機械語をエミュレータで走らせる。決めた番地に来たら OCaml の関数を呼ぶ「わな」を置ける。
   BIOS や extern 関数の代わりをさせるのに使う。 *)

exception Timeout

(* traps: (番地, 処理)。処理のあと RET したことにする。HALT で止まる。 *)
let exec ?(limit = 100_000_000) ~org ~start bin traps =
  let s = Z80.make () in
  Z80.load s org bin;
  s.pc <- start;
  s.sp <- 0xf000;
  while not s.halt do
    if s.t > limit then raise Timeout;
    match List.assoc_opt s.pc traps with
    | Some h -> h s; s.pc <- Z80.pop s
    | None -> Z80.step s
  done;
  s

(* extern 関数の表から、わなを作る。引数をレジスタから集めて ext を呼び、戻り値を A か HL に入れる。 *)
let traps exts (ext : string -> int list -> int) =
  let get (s : Z80.t) r =
    match r with
    | "bc" -> Z80.rp s 0
    | "de" -> Z80.rp s 2
    | "hl" -> Z80.rp s 4
    | r -> s.r.(String.index "bcdehl_a" r.[0])
  in
  List.map
    (fun (name, (regs, ret, addr, _)) ->
      ( addr,
        fun s ->
          let v = ext name (List.map (fun (_, r) -> get s r) regs) in
          match ret with
          | Ast.Unit -> ()
          | Ast.U16 | Ast.I16 | Ast.Ptr _ -> Z80.set_rp s 4 v
          | _ -> Z80.seta s v ))
    exts
