(* 添字をポインタに直す。

     var sp: u8 = 0
     loop { ... stack[sp] = x  sp += 1 ... stack[sp - 1] ... }

   sp が「stack を引く」「定数を足し引きする」ことにしか使われていないなら、sp の代わりに
   番地 stack + sp を変数にする。stack[sp] は「番地を計算して読む」から「そのまま読む」になり、
   sp += 1 は番地の inc になる。

   変数は一度しか束縛されないので、調べるのは次の 3 つでつながった変数の仲間（継続の引数と、
   そこへ渡す値と、定数を足し引きした結果）。仲間の全員が上の使われ方だけをしていれば、
   全員を 8bit の添字から 16bit の番地に取り替える。名前はそのまま、型だけが変わる。

   前提: 添字が表の外を指さないこと。8bit の添字は 255 の次が 0 に戻るが、番地は戻らない。
   256 バイトちょうどの表は、添字が一周することを当てにした書き方（リングバッファ）がありうるので
   対象にしない。 *)

open Ir

type def = Param | Step of string * int        (* 継続の引数 / 仲間の変数 + 定数 *)

let fn (size : string -> int option) (f : fn) : fn =
  let def = Hashtbl.create 64 and bad = Hashtbl.create 16 and table = Hashtbl.create 16 in
  let up = Hashtbl.create 64 and konts = Hashtbl.create 16 in
  let rec find x = match Hashtbl.find_opt up x with Some p when p <> x -> find p | _ -> x in
  let join a b = let a = find a and b = find b in if a <> b then Hashtbl.replace up a b in
  let other = function V x -> Hashtbl.replace bad x () | _ -> () in
  let passed = Hashtbl.create 16 in            (* 継続へ渡される変数 *)
  let signed o c = let d = (if o = "+" then c else -c) land 0xff in if d > 127 then d - 256 else d in
  (* 継続の引数を先に集める（飛ぶ側が定義より前に出てくることがある） *)
  let rec params = function
    | LetK (ks, b) ->
      List.iter
        (fun (k : kont) ->
          Hashtbl.replace konts k.k k.ps;
          List.iter (fun (p, t) -> if t = U8 then Hashtbl.replace def p Param) k.ps;
          params k.body)
        ks;
      params b
    | Let (_, _, _, k) | Store (_, _, _, k) | Out (_, _, k) | Call (_, _, _, k) | Yield k -> params k
    | If (_, _, _, _, a, b) -> params a; params b
    | Jump _ | Switch _ | Ret _ -> ()
  in
  params f.body;
  (* 定義と使われ方を調べる *)
  let rec scan = function
    | Let (x, U8, Bin ((("+" | "-") as o), V u, C c), k) -> Hashtbl.replace def x (Step (u, signed o c)); join x u; scan k
    | Let (_, U16, Idx (s, V v), k) ->
      (match Hashtbl.find_opt table (find v) with
       | Some s' when s' <> s -> other (V v)
       | _ -> Hashtbl.replace table v s);
      scan k
    | Let (_, _, e, k) -> List.iter other (Live.atoms_of e); scan k
    | Store (_, a, v, k) -> other a; other v; scan k
    | Out (_, v, k) -> other v; scan k
    | Call (_, _, args, k) -> List.iter (fun (a, _) -> other a) args; scan k
    | Yield k -> scan k
    | LetK (ks, b) -> List.iter (fun (k : kont) -> scan k.body) ks; scan b
    | Jump (k, args) ->
      List.iter2
        (fun (p, t) a ->
          match t, a with
          | U8, V v -> join p v; Hashtbl.replace passed v ()
          | U8, C _ -> ()
          | _ -> other a)
        (Hashtbl.find konts k) args
    | If (_, _, a, b, x, y) -> other a; other b; scan x; scan y
    | Switch (a, _, _) -> other a
    | Ret r -> Option.iter (fun (a, _) -> other a) r
  in
  scan f.body;
  (* 仲間ごとに: 全員が条件に合い、引く表が 1 つで、256 バイト未満なら、その表の名前 *)
  let webs = Hashtbl.create 16 in
  let all = Hashtbl.fold (fun x _ l -> x :: l) up (Hashtbl.fold (fun x _ l -> x :: l) def []) in
  List.iter (fun x -> let r = find x in Hashtbl.replace webs r (x :: Option.value ~default:[] (Hashtbl.find_opt webs r))) all;
  let target = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ xs ->
      let ts = List.sort_uniq compare (List.filter_map (Hashtbl.find_opt table) xs) in
      match ts with
      | [ s ] when List.for_all (fun x -> Hashtbl.mem def x && not (Hashtbl.mem bad x)) xs
                   && (match size s with Some n -> n < 256 | None -> false) ->
        List.iter (fun x -> Hashtbl.replace target x s) xs
      | _ -> ())
    webs;
  if Hashtbl.length target = 0 then f
  else begin
    let n = ref 0 and addr = Hashtbl.create 16 in
    let fresh () = incr n; Printf.sprintf "at.%d" !n in
    let conv x = Hashtbl.mem target x in
    (* 番地として持ち歩くのは、継続の引数と、継続へ渡す変数だけ。それ以外（sp - 1 のような途中の値）は
       変数にせず、持ち歩いている変数からのずれとして、使う所で計算し直す。 *)
    let rec base x =
      match Hashtbl.find_opt def x with
      | Some (Step (u, d)) when not (Hashtbl.mem passed x) -> let b, o = base u in b, o + d
      | _ -> x, 0
    in
    let at x = function
      | 0 -> Atom (V x)
      | d -> Bin ((if d < 0 then "-" else "+"), V x, C (abs d))
    in
    (* ポインタの向き: 持ち歩く番地を、いちばんよく読み書きする位置に合わせてずらす。
       スタックなら sp は「次に積む場所」だが、読み書きが多いのは一番上の sp - 1 なので、stack + sp - 1 を持ち歩く。
       仲間ごとに、読み書きのずれを数えて、真ん中の値（ずらす量の合計が最小になる）を選ぶ。 *)
    let uses = Hashtbl.create 16 and idx = Hashtbl.create 16 in
    let rec count = function
      | Let (m, U16, Idx (_, V v), k) when conv v -> Hashtbl.replace idx m (find v, snd (base v)); count k
      | Let (_, _, Load (V m), k) | Store (_, V m, _, k) when Hashtbl.mem idx m ->
        let r, d = Hashtbl.find idx m in
        Hashtbl.replace uses r (d :: Option.value ~default:[] (Hashtbl.find_opt uses r));
        count k
      | Let (_, _, _, k) | Store (_, _, _, k) | Out (_, _, k) | Call (_, _, _, k) | Yield k -> count k
      | LetK (ks, b) -> List.iter (fun (k : kont) -> count k.body) ks; count b
      | If (_, _, _, _, x, y) -> count x; count y
      | Jump _ | Switch _ | Ret _ -> ()
    in
    count f.body;
    let bias x =
      match Hashtbl.find_opt uses (find x) with
      | Some ds -> List.nth (List.sort compare ds) ((List.length ds - 1) / 2)
      | None -> 0
    in
    (* 番地を動かす順番を、読み書きに合わせて入れ替える。どちらも「ちょうど指している番地」で読み書きするため。
       - 先に動かす: stack[sp] = x  sp += 1 は、番地を進めてから、進めた先へ書く（inc de / ld (de),a）。
       - 後で動かす: sp -= 1 の後で前の位置を読むなら、動かす前に読んでおく（ld a,(de) / dec de）。 *)
    let early = Hashtbl.create 8 in             (* 先に動かして、定義を済ませた変数 *)
    let carried x = conv x && Hashtbl.mem passed x && not (Hashtbl.mem early x) in
    (* この先（読み書きと計算だけが並ぶ間）で、b + d ちょうどへ動く変数 *)
    let rec ahead b d = function
      | Let (x, U8, Bin (o, V u, C c), _) when carried x && base u = (b, d - signed o c) -> Some x
      | Let (_, _, _, k) | Store (_, _, _, k) -> ahead b d k
      | _ -> None
    in
    (* x へ動く前の位置 b から読む方が近い読みを、抜き出す。戻り値は (抜き出した読み, 残り) *)
    let rec before x near idx = function
      | Let (m, U16, (Idx (_, V v) as e), k) when conv v && fst (base v) = x ->
        let l, k = before x near ((m, snd (base v) - bias v) :: idx) k in
        l, Let (m, U16, e, k)
      | Let (y, t, Load (V m), k) when (match List.assoc_opt m idx with Some d -> abs (near d) < abs d | None -> false) ->
        let l, k = before x near idx k in
        (y, t, near (List.assoc m idx)) :: l, k
      | Let (y, t, e, k) -> let l, k = before x near idx k in l, Let (y, t, e, k)
      | k -> [], k
    in
    (* 読み書き 1 つ。この先でちょうどその番地へ動く変数があれば、先に動かして、それで指す *)
    let rec access m k node =
      let b, d = Hashtbl.find addr m in
      match (if d = 0 then None else ahead b d k) with
      | Some x ->
        Hashtbl.replace early x ();
        Hashtbl.replace addr m (x, 0);
        Let (x, U16, at b d, node (V x) (go k))
      | None -> let m' = fresh () in Let (m', U16, at b d, node (V m') (go k))
    and go = function
      | Let (x, U8, Bin (_, V _, C _), k) when Hashtbl.mem early x -> go k
      | Let (x, U8, Bin (o, V u, C c), k) when carried x ->
        let b, d = base u in
        let s = signed o c in
        let loads, k = before x (fun dx -> d + s + dx) [] k in
        List.fold_right
          (fun (y, t, off) rest -> let m = fresh () in Let (m, U16, at b off, Let (y, t, Load (V m), rest)))
          loads
          (Let (x, U16, at b (d + s), go k))
      | Let (x, U8, Bin (o, V u, C c), k) when conv x ->
        let b, d = base u in
        Let (x, U16, at b (d + signed o c), go k)
      | Let (m, U16, Idx (_, V v), k) when conv v ->
        let b, d = base v in
        let d = d - bias v in                   (* 持ち歩いている番地から、読み書きする番地までのずれ *)
        Hashtbl.replace addr m (b, d);
        Let (m, U16, at b d, go k)
      (* 番地は読み書きのたびに計算し直す。直前にあれば、コード生成が読み書きと 1 つにまとめられる *)
      | Let (x, t, Load (V m), k) when Hashtbl.mem addr m -> access m k (fun a k -> Let (x, t, Load a, k))
      | Store (t, V m, v, k) when Hashtbl.mem addr m -> access m k (fun a k -> Store (t, a, v, k))
      | Let (x, t, e, k) -> Let (x, t, e, go k)
      | Store (t, a, v, k) -> Store (t, a, v, go k)
      | Out (p, v, k) -> Out (p, v, go k)
      | Call (r, g, args, k) -> Call (r, g, args, go k)
      | Yield k -> Yield (go k)
      | LetK (ks, b) ->
        LetK (List.map (fun (k : kont) ->
                { k with ps = List.map (fun (p, t) -> p, if conv p then U16 else t) k.ps; body = go k.body }) ks,
              go b)
      | Jump (k, args) ->
        (* 定数の添字を渡していた所は、表の番地 + 定数を渡す *)
        Jump (k, List.map2 (fun (p, _) a -> match a with C c when conv p -> S (Hashtbl.find target p, c + bias p) | a -> a)
                   (Hashtbl.find konts k) args)
      | If (o, t, a, b, x, y) -> If (o, t, a, b, go x, go y)
      | (Switch _ | Ret _) as t -> t
    in
    { f with body = go f.body }
  end
