(* 生のバイナリを指定の番地に置いて HALT まで走らせ、時間と結果のメモリを表示する。
     runbin file.bin 0x4000 [開始番地] *)
let () =
  let ic = open_in_bin Sys.argv.(1) in
  let bin = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let org = int_of_string Sys.argv.(2) in
  let start = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else org in
  (* MSX の文字出力 (CHPUT, 0xa2) は、出た文字のコードを足し合わせるだけ *)
  let n = ref 0 and sum = ref 0 in
  let chput (s : Z80.t) = incr n; sum := !sum + s.r.(7) in
  let s = Hlang.Run.exec ~org ~start bin [ 0xa2, chput ] in
  if !n > 0 then Printf.printf "%d T, %d 文字（合計 %d）\n" s.t !n !sum else
  let b a = Char.code (Bytes.get s.m a) in
  Printf.printf "%d T, mem[0xc200]=%d mem16[0xc202]=%d\n" s.t (b 0xc200) (b 0xc202 lor (b 0xc203 lsl 8))
