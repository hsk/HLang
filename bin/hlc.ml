(* hlang コンパイラのコマンド。
     hlc file.hl            file.rom を作る
     hlc file.hl --asm      アセンブリを表示
     hlc file.hl --ir       中間表現を表示
     hlc file.hl --list     番地と機械語つきの一覧を表示
     hlc file.hl --run      エミュレータで走らせ、MSX の文字出力 (CHPUT) を表示
     hlc file.hl --report   関数ごとの大きさ、呼び出し規約、時間の上限、RAM の使い道を表示
     -O0 を付けると最適化しない（変数をすべてメモリに置く） *)

open Hlang

let () =
  let file = ref "" and mode = ref "rom" and opt = ref true in
  Array.iteri
    (fun i a ->
      if i > 0 then
        if a = "-O0" then opt := false
        else if String.length a > 2 && String.sub a 0 2 = "--" then mode := String.sub a 2 (String.length a - 2)
        else file := a)
    Sys.argv;
  if !file = "" then (prerr_endline "使い方: hlc file.hl [--asm|--ir|--list|--run]"; exit 2);
  let slurp f =
    let ic = open_in_bin f in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
  in
  (* import "x.hl" は、元のファイルの隣、その隣の lib/、環境変数 HLANG_LIB の順に探す *)
  let dir = Filename.dirname !file in
  let read f =
    let dirs = [ dir; Filename.concat dir "lib"; Filename.concat (Filename.concat dir Filename.parent_dir_name) "lib" ]
               @ Option.to_list (Sys.getenv_opt "HLANG_LIB") in
    match List.find_opt Sys.file_exists (List.map (fun d -> Filename.concat d f) dirs) with
    | Some p -> slurp p
    | None -> raise (Ast.Error (0, "import するファイルがない: " ^ f))
  in
  match compile ~opt:!opt ~read (slurp !file) with
  | exception Ast.Error (line, msg) ->
    let f, line = where line in
    Printf.eprintf "%s:%d: %s\n" (if f = "" then !file else f) line msg;
    exit 1
  | r ->
    (match !mode with
     | "asm" -> print_string (Asm.listing r.asm)
     | "ir" -> List.iter (fun f -> print_string (Ir.show f); print_newline ()) r.ir
     | "list" -> print_string (Dis.listing ~org:r.org r.bin)
     | "report" ->
       let up = String.uppercase_ascii in
       let home h = if Gen.is_mem h then "メモリ" else up h in
       Printf.printf "ROM %d バイト、RAM %d バイト\n\n" (String.length r.bin)
         (List.fold_left (fun n (_, k) -> n + k) 0 !Alloc.ram);
       List.iter
         (fun (row : Alloc.row) ->
           let f = row.fn in
           let pool = List.find_opt (fun (t, _, _) -> t = f.name) r.prog.pools in
           Printf.printf "%s %s: %d バイト\n" (if f.task then "task" else "fn") f.name row.bytes;
           if f.ps <> [] && not f.task then
             Printf.printf "  引数: %s\n"
               (String.concat ", " (List.map2 (fun (p, _) h -> Filename.remove_extension p ^ " = " ^ home h) f.ps row.homes));
           (match pool with
            | Some (_, n, size) ->
              Printf.printf "  フレーム: %d バイト × %d 個（フレームの番地は %s で持つ）\n" size n
                (home (List.hd row.homes))
            | None -> ());
           if not f.task then Printf.printf "  壊すレジスタ: %s\n" (String.concat " " (List.map up row.clobbers));
           Printf.printf "  %s: %s\n" (if f.task then "再開から次の中断まで" else "時間の上限")
             (match row.time with
              | Report.Known (t, false) -> Printf.sprintf "%d T" t
              | Report.Known (t, true) -> Printf.sprintf "%d T + 外の関数の時間" t
              | Report.Unknown -> "不明（終わりの決まらないループか、上限の分からない呼び出しがある）"))
         !Alloc.report;
       Printf.printf "\nRAM の使い道:\n";
       List.iter (fun (x, k) -> Printf.printf "  %-24s %d バイト\n" x k) !Alloc.ram
     | "run" ->
       let s = run r ~ext:(fun name args ->
         (match name, args with
          | "chput", [ c ] -> print_char (Char.chr c)
          | _ -> ());
         0)
       in
       Printf.printf "\n[%d T ステート, %d バイト]\n" s.t (String.length r.bin)
     | _ ->
       let out = Filename.remove_extension !file ^ ".rom" in
       let oc = open_out_bin out in
       output_string oc (rom r);
       close_out oc;
       Printf.printf "%s: %d バイト\n" out (String.length r.bin))
