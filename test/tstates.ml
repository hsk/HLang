(* すべての公式命令について「バイト列 最短T 最長T 命令」を出す。他の資料との突き合わせ用。 *)
open Hlang

let () =
  List.iter
    (fun pre ->
      for op = 0 to 255 do
        let src = pre ^ String.make 1 (Char.chr op) ^ "\x12\x34\x56" in
        match Dis.dis (fun a -> Char.code src.[a]) 0 with
        | Asm.Ins (o, args), len ->
          (* 相対ジャンプの行き先は番地なので、0x100 に置いたときの番地に直す *)
          let ins = match Dis.dis (fun a -> Char.code src.[a - 0x100]) 0x100 with
            | Asm.Ins (o, a), _ -> o, a | _ -> o, args in
          let lo, hi = Cost.time ins in
          let hex = String.concat "" (List.init len (fun i -> Printf.sprintf "%02x" (Char.code src.[i]))) in
          Printf.printf "%s %d %d %s\n" hex lo hi (Asm.show_ins (o, args))
        | _ -> ()
      done)
    [ ""; "\xcb"; "\xed"; "\xdd"; "\xfd"; "\xdd\xcb\x05"; "\xfd\xcb\xfb" ]
