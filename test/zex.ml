(* CP/M 用の命令網羅テスト zexdoc.com / zexall.com を走らせる。
   0 番地に来たら終了。5 番地の呼び出しは CP/M の文字出力として扱う。 *)

let () =
  let s = Z80.make () in
  let ic = open_in_bin Sys.argv.(1) in
  Z80.load s 0x100 (really_input_string ic (in_channel_length ic));
  close_in ic;
  Z80.load s 0 "\xd3\x00";                 (* OUT (0),A : 終了 *)
  Z80.load s 5 "\xdb\x00\xc9";             (* IN A,(0) : RET : 文字出力 *)
  let fin = ref false in
  s.out <- (fun _ _ -> fin := true);
  s.inp <- (fun _ ->
    let de = Z80.rp s 2 in
    (match s.r.(1) with
     | 2 -> print_char (Char.chr s.r.(3))
     | 9 ->
       let e = String.index_from (Bytes.to_string s.m) de '$' in
       print_string (Bytes.sub_string s.m de (e - de))
     | _ -> ());
    flush stdout;
    0xff);
  s.pc <- 0x100;
  while not !fin do Z80.step s done;
  Printf.printf "\n%d T\n" s.t
