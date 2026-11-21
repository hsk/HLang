(* アセンブラと逆アセンブラの検査。
   1. すべての命令コードを逆アセンブルし、組み立て直して元のバイト列に戻ることを確かめる。
   2. 同じ命令をテキストにして外部の z80asm でも組み立て、同じバイト列になることを確かめる。 *)

open Hlang

let fill = "\x12\x34\x56"                             (* オペランドに使うバイト *)
let prefixes = [ ""; "\xcb"; "\xed"; "\xdd"; "\xfd"; "\xdd\xcb\x05"; "\xfd\xcb\xfb" ]

let () =
  let fails = ref 0 and lines = ref [] in
  List.iter
    (fun pre ->
      for op = 0 to 255 do
        let src = pre ^ String.make 1 (Char.chr op) ^ fill in
        match Dis.dis (fun a -> Char.code src.[a]) 0 with
        | (Asm.Ins _ as line), len ->
          let want = String.sub src 0 len in
          let got = try fst (Asm.assemble [ line ]) with Asm.Error m -> m in
          if got <> want then begin
            incr fails;
            Printf.printf "NG %s: %s\n" (Asm.show line)
              (String.concat " " (List.map (fun c -> Printf.sprintf "%02x" (Char.code c))
                 (List.of_seq (String.to_seq want))))
          end;
          lines := (line, want) :: !lines
        | _ -> ()
      done)
    prefixes;
  let lines = List.rev !lines in
  Printf.printf "asm: 往復 %d 命令\n" (List.length lines);
  (* 外部の z80asm との突き合わせ。入っていなければ飛ばす。 *)
  if (not Sys.win32) && Sys.command ("which z80asm > " ^ Filename.null ^ " 2>&1") = 0 then begin
    let src = Filename.temp_file "hl" ".asm" and out = Filename.temp_file "hl" ".bin" in
    let bad = ref 0 in
    (* z80asm は R800 の命令（mulub、muluw）を知らないので、それは比べない *)
    let lines = List.filter (function Asm.Ins (("mulub" | "muluw"), _), _ -> false | _ -> true) lines in
    List.iter
      (fun (line, want) ->
        let oc = open_out src in
        output_string oc (Asm.show line ^ "\n");
        close_out oc;
        let ok = Sys.command (Printf.sprintf "z80asm -o %s %s 2> %s" (Filename.quote out) (Filename.quote src) Filename.null) = 0 in
        let got =
          if not ok then ""
          else begin
            let ic = open_in_bin out in
            let b = really_input_string ic (in_channel_length ic) in
            close_in ic;
            b
          end
        in
        if got <> want then begin
          incr bad;
          Printf.printf "z80asm と不一致: %s\n" (Asm.show line)
        end)
      lines;
    Printf.printf "asm: z80asm と照合、不一致 %d\n" !bad;
    fails := !fails + !bad
  end;
  (* 3. 「命令が書き換えるレジスタ」の表を、エミュレータで実際に走らせて確かめる。
     値を変えて何度か走らせ、変わったレジスタがすべて表に入っていること。 *)
  let names = [| "b"; "c"; "d"; "e"; "h"; "l"; ""; "a" |] in
  let st = Random.State.make [| 1 |] in
  List.iter
    (fun (line, bin) ->
      match line with
      | Asm.Ins ((op, _) as ins) when not (List.mem op [ "call"; "rst"; "halt"; "exx" ]) ->
        let w = Asm.writes ins in
        for _ = 1 to 20 do
          let s = Z80.make () in
          Bytes.iteri (fun i _ -> Bytes.set s.m i (Char.chr (Random.State.int st 256))) s.m;
          Z80.load s 0x100 bin;
          s.pc <- 0x100;
          s.sp <- 0x8000;
          Array.iteri (fun i _ -> s.r.(i) <- Random.State.int st 256) s.r;
          let old = Array.copy s.r in
          Z80.step s;
          Array.iteri
            (fun i name ->
              if name <> "" && s.r.(i) <> old.(i) && not (List.mem name w) then begin
                incr fails;
                Printf.printf "writes に %s がない: %s\n" name (Asm.show line)
              end)
            names
        done
      | _ -> ())
    lines;
  if !fails > 0 then exit 1
