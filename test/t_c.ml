(* C フロントエンドのテスト。test/c/*.c をコンパイルし、参照インタプリタとエミュレータで走らせた出力を、
   clang で作った test/c/*.out と比べる。最適化なしと最適化ありの両方で。
   アセンブラ（__asm）のあるもの（asm.c）は、参照インタプリタでは動かせないので比べない。asm.out は手で書いた。 *)

open Hlang

let dir = "c"

let slurp f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let read p = if Sys.file_exists p then Some (slurp p) else None

(* out は 1 文字、outh は 16 進 4 桁と空白 *)
let collector () =
  let b = Buffer.create 64 in
  let ext name args =
    (match name, args with
     | "out", [ c ] -> Buffer.add_char b (Char.chr (c land 0xff))
     | "outh", [ v ] -> Printf.bprintf b "%04x " v
     | _ -> ());
    0
  in
  b, ext

let () =
  let cs = List.sort compare (List.filter (fun f -> Filename.check_suffix f ".c") (Array.to_list (Sys.readdir dir))) in
  let bad = ref 0 in
  List.iter
    (fun f ->
      let path = Filename.concat dir f in
      let want = slurp (Filename.remove_extension path ^ ".out") in
      let compile opt = compile_c ~opt ~read ~hl_read:(fun h -> slurp (Filename.concat dir h)) ~incs:[] [ path ] in
      let check what got = if got <> want then (incr bad; Printf.printf "%s（%s）が違う:\n  期待 %S\n  結果 %S\n" f what want got) in
      match compile false, compile true with
      | exception Ast.Error (l, m) -> incr bad; let file, l = where l in Printf.printf "%s:%d: %s\n" file l m
      | r0, r1 ->
        let b, ext = collector () in
        let w = { Interp.mem = Bytes.make 0x10000 '\000'; ext; inp = (fun _ -> 0); out = (fun _ _ -> ()); fuel = 2_000_000 } in
        (* アセンブラの関数があると、参照インタプリタでは動かせない *)
        if r0.prog.asms = [] then
          (match Interp.run r0.prog w with () -> check "インタプリタ" (Buffer.contents b) | exception Interp.Out_of_fuel -> check "インタプリタ" "（長すぎる）");
        List.iter
          (fun (what, r) ->
            let b, ext = collector () in
            match run r ~ext with
            | _ -> check what (Buffer.contents b)
            | exception Run.Timeout -> check what "（止まらない）")
          [ "最適化なし", r0; "最適化あり", r1 ])
    cs;
  Printf.printf "c: %d 本%s\n" (List.length cs) (if !bad = 0 then "、すべて一致" else Printf.sprintf "、%d 件違う" !bad);
  if !bad > 0 then exit 1
