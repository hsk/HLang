(* match の出し方（順に比べる・2 分探索・表）を、枝の数を変えながらエミュレータで測る。
     dune exec test/sw.exe          値が 0 から続き番号
     dune exec test/sw.exe -- 9     値が 9 おき（とびとび）
   n 個の命令を同じ回数ずつ実行するバイトコードマシンを作り、命令 1 つあたりの T ステートと
   全体のバイト数を出す。「自動」がコンパイラの選んだもの。 *)
open Hlang

let rounds = 8

let src n step =
  let b = Buffer.create 1024 in
  let p fmt = Printf.bprintf b fmt in
  let ops = List.init n (fun i -> Printf.sprintf "O%d" i) in
  p "enum Op { %s }\n" (String.concat ", " (List.mapi (fun i o -> Printf.sprintf "%s = %d" o (i * step)) ops));
  let body = List.filteri (fun i _ -> i < n - 1) ops in
  p "data code: u8 = [ %s ]\n" (String.concat ", " (List.concat (List.init rounds (fun _ -> body)) @ [ List.nth ops (n - 1) ]));
  p "fn run(p: u16) -> u8 {\n  var acc: u8 = 0\n  loop {\n    match next(p) as Op {\n";
  List.iter (fun o -> p "      %s => { acc += 1 }\n" o) body;
  p "      O%d => { return acc }\n    }\n  }\n}\nfn main() { mem[0xc000] = run(code) }\n" (n - 1);
  Buffer.contents b

let measure n step way =
  Gen.only := way;
  match compile (src n step) with
  | exception _ -> None                          (* その出し方では出せない *)
  | r ->
    let s = run r ~ext:(fun _ _ -> 0) in
    assert (Char.code (Bytes.get s.m 0xc000) = rounds * (n - 1));
    Some (float s.t /. float (rounds * (n - 1) + 1), String.length r.bin)

let () =
  let step = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1 in
  Printf.printf "| 枝 | 順に比べる | 2 分探索 | 表 | 自動 |\n| --- | --- | --- | --- | --- |\n";
  List.iter
    (fun n ->
      let cell = function Some (t, b) -> Printf.sprintf "%.1f T / %d B" t b | None -> "—" in
      let ways = List.map (fun w -> measure n step (Some w)) [ "lin"; "bin"; "tab" ] in
      let auto = measure n step None in
      let name =
        match List.find_opt (fun (_, m) -> m = auto) (List.combine [ "順"; "2 分"; "表" ] ways) with
        | Some (w, _) -> w
        | None -> cell auto
      in
      Printf.printf "| %d | %s | %s |\n%!" n (String.concat " | " (List.map cell ways)) name)
    (List.filter (fun n -> (n - 1) * step < 256) [ 2; 3; 4; 5; 6; 7; 8; 10; 12; 16; 24; 32 ])
