(* hlcc: C（SDCC の書き方のサブセット）を hlang のバックエンドでコンパイルする。
     hlcc main.c App.c ...       main.rom を作る（ファイルはまとめて 1 つのプログラムになる）
     --asm  --list  --syms  --run   hlc と同じ
     -Ipath   #include "..." を探す所を足す
     -O0      最適化しない
     -sms     セガ・マスターシステムの ROM（.sms）
   #include "msx.hl" のように .hl を取り込むと、hlang のライブラリの関数を C から呼べる。 *)

open Hlang

let () =
  let files = ref [] and mode = ref "rom" and opt = ref true and target = ref msx and incs = ref [] in
  let rec args = function
    | [] -> ()
    | "-O0" :: r -> opt := false; args r
    | "-sms" :: r -> target := sms; args r
    | a :: r when String.starts_with ~prefix:"-I" a -> incs := !incs @ [ String.sub a 2 (String.length a - 2) ]; args r
    | a :: r when String.starts_with ~prefix:"--" a -> mode := String.sub a 2 (String.length a - 2); args r
    | a :: r -> files := !files @ [ a ]; args r
  in
  args (List.tl (Array.to_list Sys.argv));
  if !files = [] then (prerr_endline "使い方: hlcc file.c ... [-Ipath] [-O0] [-sms] [--asm|--list|--syms|--run]"; exit 2);
  let slurp f =
    let ic = open_in_bin f in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
  in
  let read p = if Sys.file_exists p && not (Sys.is_directory p) then Some (slurp p) else None in
  (* .hl は hlc と同じく、最初のファイルの隣、その隣の lib/、環境変数 HLANG_LIB の順に探す *)
  let dir = Filename.dirname (List.hd !files) in
  let hl_read f =
    let dirs = [ dir; Filename.concat dir "lib"; Filename.concat (Filename.concat dir Filename.parent_dir_name) "lib" ]
               @ !incs @ Option.to_list (Sys.getenv_opt "HLANG_LIB") in
    match List.find_opt Sys.file_exists (List.map (fun d -> Filename.concat d f) dirs) with
    | Some p -> slurp p
    | None -> raise (Ast.Error (0, "取り込む hlang のファイルがない: " ^ f))
  in
  match compile_c ~target:!target ~opt:!opt ~read ~hl_read ~incs:!incs !files with
  | exception Ast.Error (line, msg) ->
    let f, l = where line in
    if f = "" then Printf.eprintf "%s\n" msg else Printf.eprintf "%s:%d: %s\n" f l msg;
    exit 1
  | r ->
    (match !mode with
     | "asm" -> print_string (Asm.listing r.asm)
     | "list" -> print_string (Dis.listing ~org:r.org r.bin)
     | "syms" -> List.iter (fun (l, a) -> Printf.printf "%04x %s\n" a l) (List.sort (fun (_, a) (_, b) -> compare a b) r.syms)
     | "run" ->
       let s = run r ~ext:(fun name args -> (match name, args with "chput", [ c ] -> print_char (Char.chr c) | _ -> ()); 0) in
       Printf.printf "\n[%d T ステート, %d バイト]\n" s.t (String.length r.bin)
     | "rom" ->
       let out = Filename.remove_extension (List.hd !files) ^ (if !target == sms then ".sms" else ".rom") in
       let oc = open_out_bin out in
       output_string oc (rom r);
       close_out oc;
       Printf.printf "%s: %d バイト\n" out (String.length r.bin)
     | m -> Printf.eprintf "知らない指定: --%s\n" m; exit 2)
