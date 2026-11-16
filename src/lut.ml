(* LUT 合成。lut fn と書いた関数を、コンパイル時に全部の引数（0〜255）で計算して表にする。
   呼び出しは表引きになり、関数そのものは出力から消える。

   計算には参照インタプリタを使う。外の状態に触る関数は表にできないので、断る。 *)

open Ast

(* 表にできるか: メモリ・ポート・大域変数・文字列に触らず、呼ぶのは他の lut fn だけ *)
let rec pure_e luts (e : ty expr) =
  let p = pure_e luts in
  match e.x with
  | Int _ | Tf _ | Var _ -> true
  | Str _ | Glob _ | Addr _ | Mem _ | Port _ | Index _ | Field _ | Running _ -> false
  | Bin (_, a, b) -> p a && p b
  | Un (_, a) | Cast (_, a) | Lut (_, a) -> p a
  | Call (f, args) -> List.mem f luts && List.for_all p args

let rec pure_s luts = function
  | Let (_, _, e) | Do e | Return (Some e) -> pure_e luts e
  | Set ({ x = Var _; _ }, e) -> pure_e luts e
  | Set _ -> false
  | If (c, t, f) -> pure_e luts c && List.for_all (pure_s luts) (t @ f)
  | While (c, b) -> pure_e luts c && List.for_all (pure_s luts) b
  | For (_, lo, hi, b) -> pure_e luts lo && pure_e luts hi && List.for_all (pure_s luts) b
  | Match (e, arms, other) ->
    pure_e luts e
    && List.for_all (pure_s luts) (List.concat_map snd arms @ Option.value ~default:[] other)
  | Break | Continue | Return None -> true
  | Yield | Wait _ | Spawn _ | Resume _ | Each _ | Stop _ -> false

(* 式の中の lut fn の呼び出しを、表引きに直す *)
let rec map_e luts (e : ty expr) : ty expr =
  let m = map_e luts in
  let x =
    match e.x with
    | Call (f, [ a ]) when List.mem f luts -> Lut (f, m a)
    | Call (f, args) -> Call (f, List.map m args)
    | Bin (o, a, b) -> Bin (o, m a, m b)
    | Un (o, a) -> Un (o, m a)
    | Mem (t, a) -> Mem (t, m a)
    | Cast (t, a) -> Cast (t, m a)
    | Lut (f, a) -> Lut (f, m a)
    | x -> x
  in
  { e with x }

let rec map_s luts s =
  let e = map_e luts and b = List.map (map_s luts) in
  match s with
  | Let (x, t, v) -> Let (x, t, e v)
  | Set (l, v) -> Set (e l, e v)
  | If (c, t, f) -> If (e c, b t, b f)
  | While (c, body) -> While (e c, b body)
  | For (x, lo, hi, body) -> For (x, e lo, e hi, b body)
  | Match (v, arms, other) -> Match (e v, List.map (fun (ps, body) -> ps, b body) arms, Option.map b other)
  | Return (Some v) -> Return (Some (e v))
  | Do v -> Do (e v)
  | Spawn (t, args) -> Spawn (t, List.map e args)
  | (Break | Continue | Return None | Yield | Wait _ | Resume _ | Each _ | Stop _) as s -> s

let apply (p : Check.prog) : Check.prog =
  let luts = List.filter_map (fun (f : ty fn) -> if f.lut then Some f.name else None) p.fns in
  if luts = [] then p
  else begin
    List.iter
      (fun (f : ty fn) ->
        if f.lut then begin
          (match f.ps, f.ret with
           | [ (_, (U8 | Enum _)) ], (U8 | U16 | Enum _) -> ()
           | _ -> err 0 "lut fn %s: 引数は 1 バイトの値 1 つ、戻り値は u8 か u16" f.name);
          if not (List.for_all (pure_s luts) f.body) then
            err 0 "lut fn %s: メモリや大域変数、他の関数に触るので表にできない" f.name
        end)
      p.fns;
    let w = { Interp.mem = Bytes.make 0x10000 '\000'; ext = (fun _ _ -> 0); inp = (fun _ -> 0);
              out = (fun _ _ -> ()); fuel = 10_000_000 } in
    let call = Interp.load p w in
    let table (f : ty fn) =
      try f.name, f.ret, List.init 256 (fun i -> call f.name [ i ] land Check.mask f.ret)
      with Interp.Out_of_fuel -> err 0 "lut fn %s: 計算が終わらない" f.name
    in
    { p with
      luts = List.filter_map (fun (f : ty fn) -> if f.lut then Some (table f) else None) p.fns;
      fns =
        List.filter_map
          (fun (f : ty fn) -> if f.lut then None else Some { f with body = List.map (map_s luts) f.body })
          p.fns }
  end
