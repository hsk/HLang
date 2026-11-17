(* 言語のテスト。同じプログラムを 2 通りに実行して、出力が一致することを確かめる:
     1. 参照インタプリタ（構文木をそのまま実行）
     2. コンパイルした機械語をエミュレータで実行
   手で書いた例に加えて、乱数で作ったプログラムでも比べる。 *)

open Hlang

let prelude = {|
extern fn putc(c: u8 @ a) @ 0x00a2 clobbers()
extern fn puth(v: u16 @ hl) @ 0x0008
|}

(* putc は「レジスタを壊さない」と宣言してある。呼び出しをまたいでレジスタに値が残る場合も試せる。 *)

(* 出力を集める extern。putc は 1 文字、puth は 16 進 4 桁。 *)
let collector () =
  let b = Buffer.create 64 in
  let ext name args =
    (match name, args with
     | "putc", [ c ] -> Buffer.add_char b (Char.chr (c land 0xff))
     | "puth", [ v ] -> Printf.bprintf b "%04x " v
     | _ -> ());
    0
  in
  b, ext

(* ポートの読み書き: 読むたびに変わる値を返し、書いた値は出力に残す（インタプリタと機械語で同じ列になるはず） *)
let ports b =
  let n = ref 0 in
  (fun p -> incr n; ((p land 0xff) + (!n * 7)) land 0xff),
  (fun p v -> Printf.bprintf b "[%x:%x]" (p land 0xff) v)

type outcome = Out of string | Skip of string | Fail of string

let by_interp (r : result) =
  let b, ext = collector () in
  let inp, out = ports b in
  let w = { Interp.mem = Bytes.make 0x10000 '\000'; ext; inp; out; fuel = 200_000 } in
  match Interp.run r.prog w with
  | () -> Out (Buffer.contents b)
  | exception Interp.Out_of_fuel -> Skip "長すぎる"

let by_emu (r : result) =
  let b, ext = collector () in
  match run r ~ext ~io:(ports b) with
  | _ -> Out (Buffer.contents b)
  | exception Run.Timeout -> Fail "エミュレータが止まらない"

let both src =
  match compile ~opt:false (prelude ^ src), compile ~opt:true (prelude ^ src) with
  | exception Ast.Error (l, m) -> Fail (Printf.sprintf "コンパイルエラー %d: %s" (l - 3) m)
  | exception Gen.Stuck m -> Skip m              (* match の出し方を絞ったときだけ起きる *)
  | exception e -> Fail ("コンパイラが落ちた: " ^ Printexc.to_string e)
  | r0, r1 ->
    (match by_interp r0 with
     | Out a ->
       (* 最適化なしと最適化ありの両方を、インタプリタと比べる *)
       let against what r =
         match by_emu r with
         | Out b when a = b -> None
         | Out b -> Some (Printf.sprintf "不一致（%s）\n  インタプリタ: %S\n  機械語      : %S" what a b)
         | Skip m | Fail m -> Some (what ^ ": " ^ m)
       in
       (match against "最適化なし" r0, against "最適化あり" r1 with
        | None, None -> Out a
        | Some m, _ | _, Some m -> Fail m)
     | x -> x)

(* ---- 手で書いた例: (名前, ソース, 期待する出力) ---- *)

let cases = [
  "式", {|
fn main() {
  var a: u8 = 200
  var b: u8 = 100
  puth(a + b)            // u16 の場所では 16bit で計算する
  puth((a + b) as u8)    // as u8 の中も 16bit で計算してから切る
  var c: u8 = a + b      // u8 の場所では 8bit（あふれは捨てる）
  puth(c)
  puth(a - b - 101)
  puth((a & 0x0f) | (b ^ 0xff))
  puth(a << 1)
  puth(a >> 3)
  var w: u16 = 0x1234
  puth(w + a)
  puth(w - 0x2000)
  puth(w << 4)
  puth(w >> 4)
  puth(~w)
  puth(-w)
  puth(w & 0xff00 | 0x0f)
  puth((w as u8) + 1)
}|}, "012c 002c 002c ffff 009b 0190 0019 12fc f234 2340 0123 edcb edcc 120f 0035 ";
  "比較と分岐", {|
fn cmp(a: u8, b: u8) {
  if a < b { putc('<') }
  if a <= b { putc('l') }
  if a > b { putc('>') }
  if a >= b { putc('g') }
  if a == b { putc('=') }
  if a != b { putc('!') }
  putc(' ')
}
fn main() {
  cmp(1, 2) cmp(2, 1) cmp(3, 3) cmp(0, 255)
  var w: u16 = 1000
  if w > 999 && w < 1001 { putc('Y') } else { putc('N') }
  if w == 0 || !(w != 1000) { putc('Y') } else { putc('N') }
  var t: bool = w >= 1000
  if t { putc('T') }
  puth(t as u8)
}|}, "<l! >g! lg= <l! YYT0001 ";
  "ループ", {|
fn main() {
  var sum: u16 = 0
  for i in 1..11 { sum += i }
  puth(sum)
  var n: u8 = 0
  loop {
    n += 1
    if n == 3 { continue }
    if n > 5 { break }
    putc('0' + n)
  }
  for i in 0..10 {
    if i & 1 == 0 { continue }
    putc('a' + i)
  }
}|}, "0037 1245bdfhj";
  "関数と大域変数", {|
const BASE = 0xc000
var count: u8
var total: u16 = 1000
fn add(a: u16, b: u8) -> u16 { count += 1  return a + b }
fn twice(x: u8) -> u8 { return x * 2 }
fn main() {
  total = add(total, twice(21))
  total = add(total, 8)
  puth(total) puth(count)
  mem[BASE] = 0x34
  mem[BASE + 1] = 0x12
  puth(mem16[BASE])
  mem16[BASE + 2] = total
  puth(mem[BASE + 3])
}|}, "041a 0002 1234 0004 ";
  "文字列", {|
fn print(s: u16) { while mem[s] != 0 { putc(mem[s]) s += 1 } }
fn main() { print("HL") print("!") print("HL") }|}, "HL!HL";
  "列挙型と match", {|
enum Color { RED, GREEN = 5, BLUE }
fn name(c: Color) -> u8 {
  match c {
    RED => { return 'R' }
    GREEN => { return 'G' }
    BLUE => { return 'B' }
  }
}
fn main() {
  putc(name(RED)) putc(name(GREEN)) putc(name(BLUE))
  puth(BLUE as u8)
  var c: Color = GREEN
  if c == GREEN { putc('=') }
  var n: u8 = 0
  for i in 0..6 {
    match i {
      0, 2, 4 => { n += 1 }
      3 => { n += 10  continue }
      _ => { }
    }
    putc('0' + i)
  }
  puth(n)
}|}, "RGB0006 =01245000d ";
  "表と配列", {|
data primes: u8 = [2, 3, 5, 7, 11]
data wide: u16 = [1000, 2000, 3000]
data text: u8 = "abc"
var buf: [u8; 4]
var big: [u16; 3]
fn main() {
  var s: u16 = 0
  for i in 0..5 { s += primes[i] }
  puth(s)
  puth(wide[1] + wide[2])
  putc(text[2])
  for i in 0..4 { buf[i] = text[0] + i }
  putc(buf[3])
  big[2] = 0x1234
  big[0] = big[2] + 1
  puth(big[0]) puth(big[1])
}|}, "001c 1388 cd1235 0000 ";
  "next で読み進める", {|
enum Op { PUT, ADD, JUMP, END }
data code: u8 = [ PUT, 'a', ADD, 2, PUT, 'a', JUMP, 11, 0, PUT, 'x', PUT, 'z', END ]
fn main() {
  var p: u16 = code
  var acc: u8 = 0
  loop {
    match next(p) as Op {
      PUT => { putc(next(p) + acc) }
      ADD => { acc += next(p) }
      JUMP => { p = code + next16(p) }
      END => { break }
    }
  }
  puth(p - code)
}|}, "ac|000e ";
  "定数倍と末尾呼び出し", {|
fn triple(x: u8) -> u8 { return x * 3 }
fn f(x: u8) -> u8 { return triple(x + 1) }
fn main() {
  var a: u8 = 7
  var w: u16 = 300
  puth(a * 10) puth(w * 10) puth(a * 255) puth(w * 1000) puth(a * 0) puth(f(a))
  var b: u8 = a * 37
  puth(b)
}|}, "0046 0bb8 06f9 93e0 0000 0018 0003 ";
  "たくさんの枝（表で飛ぶ）", {|
enum Op { A, B, C, D, E, F, G, H }
data code: u8 = [ A, B, C, D, E, F, G, A, H ]
fn main() {
  var p: u16 = code
  var n: u8 = 0
  loop {
    match next(p) as Op {
      A => { putc('a') }
      B => { putc('b') n += 1 }
      C => { putc('c') }
      D => { n += 2 }
      E => { putc('e') }
      F => { n += 4 }
      G => { putc('g') }
      H => { break }
    }
  }
  puth(n)
  for i in 0..10 {
    match i { 0 => { putc('0') } 1 => { putc('1') } 2, 3 => { putc('x') } 5 => { putc('5') } 6 => { putc('6') } 7 => { putc('7') } _ => { putc('.') } }
  }
}|}, "abcega0007 01xx.567..";
  "とびとびの値の match", {|
enum Op { A = 3, B = 4, C = 5, D = 40, E = 41, F = 90, G = 200, H = 255 }
fn kind(o: Op) -> u8 {
  match o { A => { return 'a' } B => { return 'b' } C => { return 'c' } D => { return 'd' }
            E => { return 'e' } F => { return 'f' } G => { return 'g' } H => { return 'h' } }
}
fn main() {
  putc(kind(A)) putc(kind(B)) putc(kind(C)) putc(kind(D)) putc(kind(E)) putc(kind(F)) putc(kind(G)) putc(kind(H))
  var x: u8 = 0
  var n: u16 = 0
  loop {
    // 値を match の後でも使う。同じ行き先が並ぶ所、0 と 255、どれでもない値。
    match x {
      0 => { n += 1 }
      1, 2, 3 => { n += x }
      7 => { n += 100 }
      8 => { n += 200 }
      9 => { n += 300 }
      64, 65 => { n += 1000 }
      128 => { n += x }
      254 => { n += 3000 }
      255 => { n += 5000 }
      _ => { }
    }
    x += 1
    if x == 0 { break }
  }
  puth(n)
}|}, "abcdefgh29ef ";
  "構造体", {|
struct Enemy { x: u8, y: u8, hp: u16, kind: u8 }
var boss: Enemy
var es: [Enemy; 4]
fn hit(e: *Enemy, n: u8) { e.hp = e.hp - n  e.x += 1 }
fn main() {
  boss.x = 10  boss.y = 20  boss.hp = 1000  boss.kind = 7
  hit(boss, 1)
  puth(boss.x) puth(boss.y) puth(boss.hp) puth(boss.kind)
  for i in 0..4 { var e = es[i]  e.x = i  e.y = i * 3  e.hp = 100  e.kind = i + 1 }
  hit(es[2], 5)
  var s: u16 = 0
  for i in 0..4 { var e = es[i]  s += e.x + e.y + e.hp + e.kind }
  puth(s)
  var p: *Enemy = es[3]
  puth(p.y) puth((p as u16) - (es[0] as u16))
}|}, "000b 0014 03e7 0007 01ae 0009 000f ";
  "表にした関数", {|
lut fn sq(x: u8) -> u16 { var w: u16 = x  var r: u16 = 0  for i in 0..x { r += w }  return r }
lut fn half(x: u8) -> u8 { return x >> 1 }
fn main() {
  puth(sq(12)) puth(sq(255)) puth(half(201))
  var s: u16 = 0
  for i in 0..10 { s += sq(i) }
  puth(s)
}|}, "0090 fe01 0064 011d ";
  "タスク", {|
var total: u16
task count() {
  for i in 0..3 { putc('0' + i)  yield }
  putc('!')
}
task blink() {
  var n: u8 = 0
  loop {
    putc('a' + n)
    n += 1
    wait 2                       // 2 回休む
    if n == 3 { return }
  }
}
task sum() {
  var p: u16 = 0xc000
  var acc: u16 = 0
  while p != 0xc004 { acc += mem[p]  p += 1  total = acc  yield }
}
fn step() { resume count  resume blink  resume sum }
fn h1() { }
fn h2() { }
fn main() {
  mem[0xc000] = 1  mem[0xc001] = 20  mem[0xc002] = 200  mem[0xc003] = 100
  resume count                   // まだ始めていないので何もしない
  putc(running(count) as u8 + 'N')
  spawn count()  spawn blink()  spawn sum()
  var live = running(count)
  while live || running(blink) { step()  putc('|')  live = running(count) }
  puth(total)
  spawn count()                  // もう一度最初から
  resume count  resume count
  spawn count()
  resume count
  puth((&h1 != &h2) as u8)
}|}, "N0a|1|2b|!|c|||0141 0100001 ";
  "プールのタスク（HL で歩く）", {|
task zako(ch: u8, n: u8) pool 3 {
  var i: u8 = 0
  while i != n { putc(ch + i)  i += 1  yield }
  putc('.')
}
fn main() {
  spawn zako('a', 2)
  spawn zako('A', 3)
  while running(zako) { resume zako  putc('|') }
  spawn zako('0', 1) spawn zako('1', 1) spawn zako('2', 1) spawn zako('3', 1)   // 4 つめは空きがない
  resume zako resume zako
  putc(running(zako) as u8 + '0')
}|}, "aA|bB|.C|.|012...0";
  "プールのタスク（IX を使う）", {|
enum Op { PUT, GAP, END }
data s1: u8 = [ PUT, 'x', GAP, 2, PUT, 'y', END ]
data s2: u8 = [ GAP, 1, PUT, 'p', PUT, 'q', GAP, 3, PUT, 'r', END ]
var total: u16
task track(p: u16, id: u8) pool 4 {
  var count: u16 = 0
  loop {
    match next(p) as Op {
      PUT => { putc(next(p))  count += 0x101  yield }
      GAP => { wait next(p) }
      END => { puth(count + id)  total += count  return }
    }
  }
}
task boss() {
  spawn track(s1, 1)
  wait 2
  spawn track(s2, 2)
  spawn track(s1, 3)
}
fn main() {
  spawn boss()
  resume boss
  var n: u8 = 0
  while running(track) || running(boss) { resume boss  resume track  putc('|')  n += 1 }
  puth(total) puth(n)
}|}, "x|x|p|yq|0203 y|0205 ||r|0305 |0707 0009 ";
  "割り算と余り", {|
fn digits(n: u16) { if n >= 10 { digits2(n / 10) }  putc('0' + (n % 10) as u8) }
fn digits2(n: u16) { if n >= 10 { digits3(n / 10) }  putc('0' + (n % 10) as u8) }
fn digits3(n: u16) { putc('0' + (n % 10) as u8) }
fn main() {
  var a: u8 = 200
  var b: u8 = 7
  var w: u16 = 50000
  var z: u8 = 0
  puth(a / b) puth(a % b) puth(a / 8) puth(a % 8) puth(a / a) puth(b / a) puth(b % a)
  puth(w / 300) puth(w % 300) puth(w / 16) puth(w % 16) puth(w / 40000) puth(w % 40000) puth(65535 / w)
  puth(a / z) puth(a % z) puth(w / z) puth(w % z)
  puth(1000 / 7) puth(1000 % 7)
  a /= 3  w %= 1000  puth(a) puth(w)
  digits(987)
}|}, "001c 0004 0019 0000 0001 0000 0007 00a6 00c8 0c35 0000 0001 2710 0001 ffff 00c8 ffff c350 008e 0006 0042 0000 987";
  "添字をポインタに直す", {|
var st: [u8; 16]
var q: [u8; 8]
var ring: [u8; 4]
data src: u8 = [ 3, 1, 4, 1, 5, 9, 2, 6 ]
fn main() {
  // 積んで、足して、降ろす。添字は st を引くことと ±1 にしか使わない（ポインタになる）
  var sp: u8 = 0
  for i in 0..8 { st[sp] = src[i]  sp += 1 }
  for i in 0..3 { sp -= 1  st[sp - 1] = st[sp - 1] + st[sp] }
  while st[sp - 1] != 3 { sp -= 1  puth(st[sp]) }
  // 0 でない所から始めて、2 つずつ進む
  var k: u8 = 1
  for i in 0..3 { q[k] = i + 10  q[k - 1] = q[k] + 1  k += 2 }
  for i in 0..6 { puth(q[i]) }
  // 添字を比べたり表示したりする（ポインタにはできない）。結果は同じはず
  var r: u8 = 0
  for i in 0..10 { ring[r] = i  r += 1  if r == 4 { r = 0 } }
  puth(ring[0]) puth(ring[1]) puth(ring[3]) puth(r)
}|}, "0016 0001 0004 0001 000b 000a 000c 000b 000d 000c 0008 0009 0007 0002 ";
  "ポインタの向きと、動かす順番", {|
var st: [u8; 16]
fn main() {
  var sp: u8 = 0
  for i in 0..5 { st[sp] = i * 3  sp += 1 }            // 積む: 進めてから書く形になる
  st[sp] = st[sp - 1]  sp += 1                          // 一番上を複製
  sp -= 1  st[sp - 1] = st[sp - 1] - st[sp]             // 引く: 戻す前に読む形になる
  st[sp] = 7  st[sp + 1] = 8  sp += 2                   // 2 つ積む
  while sp != 0 { sp -= 1  puth(st[sp]) }
  var k: u8 = 8
  for i in 0..4 { k -= 2  st[k] = i  st[k + 1] = i + 10 }   // 下へ伸びる
  for i in 0..8 { puth(st[i]) }
}|}, "0008 0007 0000 0009 0006 0003 0000 0003 000d 0002 000c 0001 000b 0000 000a ";
  "個体を外から見る・止める", {|
task zako(id: u8, x: u8, hp: u8) pool 4 {
  loop { x += 1  if hp == 0 { putc('x')  return }  yield }
}
task shot(x: u8) pool 2 {
  var life: u8 = 3
  while life != 0 { life -= 1  yield }
}
task solo() { loop { putc('s')  yield } }
fn show() {
  for e in zako { putc('a' + e.id)  puth(e.x) }
  putc('|')
}
fn main() {
  show()                                  // 何も動いていない
  spawn zako(0, 10, 2)  spawn zako(1, 20, 1)  spawn zako(2, 30, 9)
  spawn shot(21)  spawn solo()
  resume zako  resume shot  resume solo
  show()
  // 弾と敵を突き合わせる。当たったら hp を減らし、弾を消す
  for b in shot {
    for e in zako {
      if e.x == b.x { e.hp -= 1  stop b  break }
    }
  }
  putc(running(shot) as u8 + '0')
  resume zako                             // id 1 は hp が 0 になって終わる
  show()
  for e in zako { if e.id == 0 { continue }  stop e }
  show()
  resume solo  stop solo  resume solo  putc(running(solo) as u8 + '0')
  stop zako
  putc(running(zako) as u8 + '0')
}|}, "|sa000b b0015 c001f |0xa000c c0020 |a000c |s00";
  "ブロック転送", {|
var buf: [u8; 8]
data src: u8 = "ABCDEFG"
fn main() {
  copy(buf, src, 7)
  copy(buf + 1, buf, 3)                   // 重なっていると、前から 1 バイトずつなので A が広がる
  var n: u16 = 4
  copy(buf + 4, src + n, n - 1)
  for i in 0..7 { putc(buf[i]) }
}|}, "AAAAEFG";
  "ブロック転送のループ", {|
var a: [u8; 16]
var b: [u8; 16]
fn fwd(p: u16, n: u8) { for i in 0..n { mem[p + 1 + i] = mem[p + i] } }           // 上へ（前から）
fn rev(p: u16, n: u8) { var i: u8 = n  while i != 0 { i -= 1  mem[p + 1 + i] = mem[p + i] } }   // 下へ（後ろから）
fn fill(p: u16, n: u8, v: u8) { for i in 0..n { mem[p + i] = v } }
fn fillrev(p: u16, n: u8, v: u8) { var i: u8 = n  while i != 0 { i -= 1  mem[p + i] = v } }
fn fwdc(p: u16) { for i in 0..5 { mem[p + 1 + i] = mem[p + i] } }
fn revc(p: u16) { var i: u8 = 6  while i != 0 { i -= 1  mem[p + 1 + i] = mem[p + i] } }
fn revk(p: u16, n: u8) -> u8 { var i: u8 = n  while i != 0 { i -= 1  mem[p + 1 + i] = mem[p + i] }  return i }
fn show(p: u16, n: u8) { for i in 0..n { putc('0' + mem[p + i]) }  putc('|') }
fn reset() { for i in 0..8 { a[i] = i } }
fn main() {
  reset()  show(&a, 8)
  rev(&a, 7)  show(&a, 8)                  // 右へ 1 つずらす。後ろから写すので壊れない
  fwd(&a, 4)  show(&a, 8)                  // 前から写すので、先頭の値が広がる
  fill(&a, 6, 9)  show(&a, 8)
  fill(&a, 0, 1)  show(&a, 8)              // 0 個は何もしない
  fill(&a + 2, 3, 2)  show(&a, 8)
  fillrev(&a, 4, 5)  show(&a, 8)
  reset()  revc(&a)  show(&a, 8)           // 回数が定数
  reset()  fwdc(&a)  show(&a, 8)
  reset()  putc('0' + revk(&a, 3))  show(&a, 8)   // 抜けた後の変数は 0
  fwd(&a, 0)  rev(&a, 0)  show(&a, 8)      // 0 個は何もしない
  for i in 0..8 { b[i] = a[i] }            // 別の配列へ
  for i in 0..8 { a[i] = 0 }
  show(&b, 8)  show(&a, 8)
}|}, "01234567|00123456|00000456|99999956|99999956|99222956|55552956|00123457|00000067|000124567|00124567|00124567|00000000|";
  "ポートへ書く・読むループ", {|
var buf: [u8; 8]
fn sendup(p: u16, n: u8) { for i in 0..n { port[0x98] = mem[p + i] } }                    // otir
fn senddown(p: u16, n: u8) { var i: u8 = n  while i != 0 { i -= 1  port[0x98] = mem[p + i] } }   // otdr
fn recvup(p: u16, n: u8) { for i in 0..n { mem[p + i] = port[0x99] } }                    // inir
fn recvdown(p: u16, n: u8) { var i: u8 = n  while i != 0 { i -= 1  mem[p + i] = port[0x99] } }  // indr
fn send5(p: u16) { for i in 0..5 { port[0x98] = mem[p + i] } }
fn recv4d(p: u16) { var i: u8 = 4  while i != 0 { i -= 1  mem[p + i] = port[0x99] } }
fn show() { for i in 0..8 { puth(buf[i]) } }
fn main() {
  for i in 0..8 { buf[i] = i + 1 }
  sendup(&buf, 4)  senddown(&buf, 3)  sendup(&buf, 0)
  recvup(&buf, 3)  recvdown(&buf + 4, 3)
  show()
  send5(&buf)  recv4d(&buf)
  show()
}|}, "[98:1][98:2][98:3][98:4][98:3][98:2][98:1]00a0 00a7 00ae 0004 00c3 00bc 00b5 0008 [98:a0][98:a7][98:ae][98:4][98:c3]00df 00d8 00d1 00ca 00c3 00bc 00b5 0008 ";
  "構造体の配列を歩くループ", {|
struct E { a: u8, w: u16, b: u8 }
var es: [E; 6]
fn sum() -> u16 {
  var s: u16 = 0
  for i in 0..6 { if es[i].a == 2 { continue }  s += es[i].w  s += es[i].b }
  return s
}
fn main() {
  for i in 1..6 { es[i].a = i  var v: u16 = i  es[i].w = v * 100  es[i].b = i + 1 }   // 1 から始まる
  puth(sum())
  for i in 0..6 { for j in 0..6 { if es[j].a == i { es[j].b += 1 } } }                // 入れ子
  puth(sum())
  for i in 0..6 { puth(es[i].b) }
}|}, "0525 052a 0001 0003 0004 0005 0006 0007 ";
  "16bit の計算を 8bit に狭める", {|
struct P { a: u8, w: u16, v: u16 }
var p: P
var w: u16
fn hi(q: *P) -> u8 { return (q.w >> 8) as u8 }
fn both(q: *P) -> u16 { var t = q.v  return (t >> 8) + t }     // 上位と全体の両方を使う
fn main() {
  var b: u8 = 0xe5
  w = 0x12f7
  p.a = 0x9c  p.w = 0xabcd  p.v = 0x1234
  var h: u16 = w
  puth((w + 0x1f0) & 0xf0)                   // 桁上がりは上位へだけ響く
  puth(((h + b) & 0xff) >> 3)
  h = p.a
  puth(((h + 0x20) & 0xc0) >> 3)
  puth((h & 8) >> 1)
  puth(((h << 1) | 1) & 0x7f)
  puth((h - 0xa0) & 0xfe)
  puth(hi(&p))
  puth(both(&p))
}|}, "00e0 001b 0010 0004 0039 00fc 00ab 1246 ";
  "符号付きの数", {|
var gv: i8 = -3
fn sign(v: i8) -> u8 { if v < 0 { return '-' }  if v == 0 { return '0' }  return '+' }
fn main() {
  var x: u8 = 100
  var dx: i8 = -2
  var w: u16 = 1000
  var d: i16 = -300
  x += dx  puth(x)                        // 符号なしに足すと、引き算になる
  w += dx  puth(w)                        // 16bit へは符号を保って広がる
  w += d   puth(w)
  putc(sign(dx)) putc(sign(0)) putc(sign(5)) putc(sign(gv)) putc(sign(-128)) putc(sign(127))
  var a: i8 = -100
  var b: i8 = 100
  puth((a < b) as u8) puth((b < a) as u8) puth((a <= -100) as u8) puth((a > -101) as u8)
  puth((d < 0) as u8) puth((d < -301) as u8) puth((d >= -300) as u8)
  var e: i16 = dx                         // -2 のまま 16bit に
  puth(e as u16) puth((e < d) as u8) puth((e > d) as u8)
  dx = -dx  puth(dx as u8)  dx -= 5  puth(dx as u8) putc(sign(dx))
  var u: u8 = 200
  puth((u > 100) as u8)                   // 符号なしどうしは今までどおり
  var i: i8 = -3
  while i < 3 { putc('a' + (i + 3) as u8)  i += 1 }
}|}, "0062 03e6 02ba -0+--+0001 0000 0001 0001 0001 0000 0001 fffe 0000 0001 0002 00fd -0001 abcdef";
  "spawn が個体を返す", {|
task zako(id: u8, x: u8) pool 2 {
  loop { putc('a' + id)  puth(x)  yield }
}
task solo() { putc('s') }
var last: *zako
fn main() {
  var a = spawn zako(0, 10)
  var b = spawn zako(1, 20)
  var c = spawn zako(2, 30)               // 空きがない: 動かない予備の枠を指す
  last = b
  a.x += 5
  b.x = a.x + c.x
  resume zako  putc('|')
  stop a
  last.x = 1
  resume zako  putc('|')
  var d = spawn zako(3, 40)               // a のいた枠が空いている
  puth(((d as u16) == (a as u16)) as u8)
  stop c                                  // 予備の枠を止めても何も起きない
  resume zako
  spawn solo()  resume solo
}|}, "a000f b002d |b0001 |0001 d0028 b0001 s";
  "符号付きのシフトと割り算", {|
fn main() {
  var a: i8 = -100
  var b: i8 = 7
  var z: i8 = 0
  var m: i8 = -128
  puth((a >> 1) as u8) puth((a >> 3) as u8) puth((a >> 7) as u8) puth((b >> 1) as u8) puth((a >> 9) as u8)
  puth((a / b) as u8) puth((a % b) as u8) puth((b / a) as u8) puth((b % a) as u8)
  puth((a / -7) as u8) puth((a % -7) as u8) puth((100 / (0 - b)) as u8) puth((a / 4) as u8) puth((a % 4) as u8)
  puth((m / -1) as u8) puth((m % -1) as u8) puth((m / 2) as u8)
  puth((a / z) as u8) puth((a % z) as u8) puth((b / z) as u8)
  var w: i16 = -30000
  var d: i16 = 7
  var zz: i16 = 0
  puth((w >> 1) as u16) puth((w >> 4) as u16) puth((w >> 8) as u16) puth((w >> 11) as u16) puth((w >> 15) as u16) puth((d >> 1) as u16)
  puth((w / d) as u16) puth((w % d) as u16) puth((d / w) as u16) puth((d % w) as u16)
  puth((w / -300) as u16) puth((w % -300) as u16) puth((w / 16) as u16) puth((w % 16) as u16)
  puth((w / zz) as u16) puth((w % zz) as u16)
  var e: i16 = a                          // 8bit の割り算の結果を 16bit へ
  e = e / 3  puth(e as u16)
  a /= 3  a >>= 1  puth(a as u8)
}|}, "00ce 00f3 00ff 0003 00ff 00f2 00fe 0000 0007 000e 00fe 00f2 00e7 0000 0080 0000 00c0 0001 009c 00ff "
   ^ "c568 f8ad ff8a fff1 ffff 0003 ef43 fffb 0000 0007 0064 0000 f8ad 0000 0001 8ad0 ffdf 00ef ";
  "入れ替え", {|
fn main() {
  var a: u8 = 1
  var b: u8 = 2
  for i in 0..3 { var t = a  a = b  b = t }
  puth(a) puth(b)
}|}, "0002 0001 ";
]

(* ---- コンパイルエラーになるべき例: (名前, ソース, メッセージに含まれる言葉) ---- *)

let errors = [
  "match の抜け", {|
enum Op { A, B, C }
fn main() { var o: Op = A  match o { A => { } C => { } } }|}, "抜けがある: B";
  "u8 の match", {|fn main() { var x: u8 = 1  match x { 1 => { } } }|}, "_ =>";
  "match の重複", {|fn main() { var x: u8 = 1  match x { 1, 1 => { } _ => { } } }|}, "2 回";
  "列挙型と数", {|
enum Op { A, B }
fn main() { var x: u8 = A }|}, "as u8";
  "再帰", {|fn f() { g() }  fn g() { f() }  fn main() { f() }|}, "再帰";
  "while の中の next", {|fn main() { var p: u16 = 0  while next(p) != 0 { } }|}, "while の条件";
  "範囲", {|fn main() { var x: u8 = 300 }|}, "入らない";
  "task の外の yield", {|fn main() { yield }|}, "task の中";
  "自分を resume", {|task t() { resume t }  fn main() { spawn t() }|}, "再帰";
  "知らないタスク", {|fn main() { spawn t() }|}, "知らないタスク";
  "割り込みと共有", {|
fn beep() { putc(7) }
fn tick() { beep() }
fn main() { mem16[0xc000] = &tick  beep() }|}, "割り込みから";
  "for in の yield", {|task t(x: u8) pool 2 { }  task u() { for e in t { yield } }  fn main() { }|}, "yield できない";
  "stop の相手", {|fn main() { var x: u8 = 1  stop x }|}, "stop できる";
  "pool のない引数", {|task t(x: u8) { }  fn main() { }|}, "pool";
  "spawn の引数", {|task t(x: u8) pool 2 { }  fn main() { spawn t() }|}, "引数は 1 個";
  "プールの for", {|task t() pool 2 { for i in 0..3 { yield } }  fn main() { }|}, "while で書く";
  "型", {|fn main() { var w: u16 = 1  var x: u8 = w }|}, "as u8";
]

(* ---- 乱数で作るプログラム ---- *)

let make seed =
  let st = Random.State.make [| seed |] in
  let int n = Random.State.int st n in
  let pick l = List.nth l (int (List.length l)) in
  let b = Buffer.create 1024 in
  let p fmt = Printf.bprintf b fmt in
  let fresh = let n = ref 0 in fun () -> incr n; Printf.sprintf "v%d" !n in
  (* vars: (名前, 型, 代入してよいか) *)
  let of_ty vars t = List.filter_map (fun (x, t', _) -> if t = t' then Some x else None) vars in
  let fns = ref [] in                          (* (名前, 引数の型, 戻りの型) *)
  (* メモリは 0xc000 からの 256 バイト（+1）だけを使う。番地の式は u16 で計算されるので、
     as u8 で 8bit に切ってから足す。そうしないとプログラム自身を読み書きしてしまう。 *)
  (* 式。左端は必ず変数にする（定数だけの式が範囲を超えてエラーになるのを避ける）。 *)
  let rec num t vars d =
    let leaf () = pick (of_ty vars t) in
    let lit () = string_of_int (int (if t = "u8" then 256 else 65536)) in
    let sub () = if int 3 = 0 then lit () else num t vars (d - 1) in
    (* 構造体のフィールド: 大域変数、配列の要素、ポインタの変数のどれかから読む *)
    let place () =
      match of_ty vars "ptr" with
      | l when l <> [] && int 2 = 0 -> pick l
      | _ -> if int 2 = 0 then "s1" else Printf.sprintf "ss[(%s & 3)]" (num "u8" vars (d - 1))
    in
    if d <= 0 then leaf ()
    else
      match int 17 with
      | 0 | 1 -> leaf ()
      | 2 | 3 | 4 -> Printf.sprintf "(%s %s %s)" (num t vars (d - 1)) (pick [ "+"; "-"; "&"; "|"; "^" ]) (sub ())
      | 5 -> Printf.sprintf "(%s %s %d)" (num t vars (d - 1)) (pick [ "<<"; ">>" ]) (int (if t = "u8" then 9 else 17) land 15)
      | 6 -> Printf.sprintf "(%s%s)" (pick [ "-"; "~" ]) (num t vars (d - 1))
      | 7 -> Printf.sprintf "(%s as %s)" (num (if t = "u8" then "u16" else "u8") vars (d - 1)) t
      | 8 -> Printf.sprintf "%s[0xc000 + (%s as u8)]" (if t = "u8" then "mem" else "mem16") (num "u8" vars (d - 1))
      | 9 when t = "u8" && int 2 = 0 ->
        Printf.sprintf "%s[(%s & 7)]" (pick [ "tbl"; "arr" ]) (num "u8" vars (d - 1))
      | 9 -> Printf.sprintf "(%s as %s)" (cond vars (d - 1)) t
      | 12 -> Printf.sprintf "(%s * %d)" (num t vars (d - 1)) (int 41)
      | 13 -> Printf.sprintf "(%s %s %s)" (num t vars (d - 1)) (pick [ "/"; "%" ]) (sub ())
      | 14 -> Printf.sprintf "%s(%s)" (if t = "u8" then "l8" else "l16") (num "u8" vars (d - 1))
      | 15 -> Printf.sprintf "%s.%s" (place ()) (if t = "u8" then pick [ "a"; "b" ] else "w")
      | 16 ->
        (* 符号付きとして計算して、ビットはそのまま戻す *)
        let s = if t = "u8" then "i8" else "i16" in
        if int 2 = 0 then Printf.sprintf "(((%s as %s) >> %d) as %s)" (num t vars (d - 1)) s (int (if t = "u8" then 9 else 17) land 15) t
        else Printf.sprintf "(((%s as %s) %s (%s as %s)) as %s)" (num t vars (d - 1)) s (pick [ "/"; "%" ]) (num t vars (d - 1)) s t
      | _ ->
        (match List.filter (fun (_, _, r) -> r = t) !fns with
         | [] -> leaf ()
         | l -> let f, ps, _ = pick l in call f ps vars d)
  and call f ps vars d =
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map (fun t -> if t = "bool" then cond vars (d - 1) else num t vars (d - 1)) ps))
  and cond vars d =
    if d <= 0 then pick (of_ty vars "bool")
    else
      match int 9 with
      | 0 -> pick (of_ty vars "bool")
      | 1 -> Printf.sprintf "(%s %s %s)" (cond vars (d - 1)) (pick [ "&&"; "||" ]) (cond vars (d - 1))
      | 2 -> "!" ^ cond vars (d - 1)
      | 3 ->
        let t = pick [ "u8"; "u16" ] in
        let s = if t = "u8" then "i8" else "i16" in
        Printf.sprintf "((%s as %s) %s (%s as %s))" (num t vars (d - 1)) s (pick [ "<"; "<="; ">"; ">=" ]) (num t vars (d - 1)) s
      | _ ->
        let t = pick [ "u8"; "u8"; "u16" ] in
        Printf.sprintf "(%s %s %s)" (num t vars (d - 1)) (pick [ "=="; "!="; "<"; "<="; ">"; ">=" ])
          (if int 3 = 0 then string_of_int (int (if t = "u8" then 256 else 65536)) else num t vars (d - 1))
  in
  let any vars d t = if t = "bool" then cond vars d else num t vars d in
  let rec block ind vars depth in_loop ret =
    let vars = ref vars in
    for _ = 1 to 2 + int 5 do
      let v = !vars in
      let writable = List.filter (fun (_, _, w) -> w) v in
      match int 17 with
      | 14 ->
        let at = match of_ty v "ptr" with
          | l when l <> [] && int 2 = 0 -> pick l
          | _ -> if int 2 = 0 then "s1" else Printf.sprintf "ss[(%s & 3)]" (num "u8" v 2)
        in
        if int 3 = 0 then p "%s%s.w = %s\n" ind at (num "u16" v 2)
        else p "%s%s.%s %s %s\n" ind at (pick [ "a"; "b" ]) (pick [ "="; "+="; "^=" ]) (num "u8" v 2)
      | 15 ->
        let x = fresh () in
        p "%svar %s: *S = %s\n" ind x (if int 3 = 0 then "s1" else Printf.sprintf "ss[(%s & 3)]" (num "u8" v 2));
        vars := (x, "ptr", false) :: v
      | 0 | 1 ->
        let x = fresh () and t = pick [ "u8"; "u8"; "u16"; "bool" ] in
        p "%svar %s: %s = %s\n" ind x t (any v 3 t);
        vars := (x, t, true) :: v
      | 2 | 3 | 4 ->
        let x, t, _ = pick writable in
        if t <> "bool" && int 3 = 0 then p "%s%s %s %s\n" ind x (pick [ "+="; "-="; "^="; "&="; "|=" ]) (num t v 2)
        else p "%s%s = %s\n" ind x (any v 3 t)
      | 5 -> p "%sputc(%s)\n" ind (num "u8" v 3)
      | 6 -> p "%sputh(%s)\n" ind (num "u16" v 3)
      | 7 ->
        if int 2 = 0 then p "%smem[0xc000 + (%s as u8)] = %s\n" ind (num "u8" v 2) (num "u8" v 2)
        else p "%smem16[0xc000 + (%s as u8)] = %s\n" ind (num "u8" v 2) (num "u16" v 2)
      | 8 | 9 when depth > 0 ->
        p "%sif %s {\n" ind (cond v 3);
        block (ind ^ "  ") v (depth - 1) in_loop ret;
        if int 2 = 0 then (p "%s} else {\n" ind; block (ind ^ "  ") v (depth - 1) in_loop ret);
        p "%s}\n" ind
      | 10 when depth > 0 ->
        let i = fresh () in
        p "%sfor %s in %d..%d {\n" ind i (int 3) (int 6);
        block (ind ^ "  ") ((i, "u8", false) :: v) (depth - 1) true ret;
        p "%s}\n" ind
      | 11 when depth > 0 ->
        let n = fresh () in
        p "%svar %s: u8 = %d\n%swhile %s > 0 {\n%s  %s -= 1\n" ind n (int 5) ind n ind n;
        block (ind ^ "  ") ((n, "u8", false) :: v) (depth - 1) true ret;
        p "%s}\n" ind
      | 12 when depth > 0 && int 2 = 0 ->
        (* match: 値をいくつかの枝に分ける。_ は付けたり付けなかったり（付けないのは列挙型だけ）。 *)
        p "%smatch (%s & 3) {\n" ind (num "u8" v 2);
        p "%s  0 => {\n" ind;
        block (ind ^ "    ") v (depth - 1) in_loop ret;
        p "%s  }\n%s  1, 2 => {\n" ind ind;
        block (ind ^ "    ") v (depth - 1) in_loop ret;
        p "%s  }\n%s  _ => { arr[(%s & 7)] = %s }\n%s}\n" ind ind (num "u8" v 2) (num "u8" v 2) ind
      | 12 when in_loop -> p "%sif %s { %s }\n" ind (cond v 2) (pick [ "break"; "continue" ])
      | 13 ->
        (match ret with
         | Some t when int 3 = 0 -> p "%sif %s { return %s }\n" ind (cond v 2) (if t = "" then "" else any v 2 t)
         | _ ->
           (match List.filter (fun (_, _, r) -> r = "") !fns with
            | [] -> ()
            | l -> let f, ps, _ = pick l in p "%s%s\n" ind (call f ps v 3)))
      | _ -> ()
    done
  in
  let globals = [ "g8", "u8", true; "g16", "u16", true; "gb", "bool", true ] in
  p "var g8: u8 = %d\nvar g16: u16 = %d\nvar gb: bool\n" (int 256) (int 65536);
  p "data tbl: u8 = [%s]\nvar arr: [u8; 8]\n" (String.concat ", " (List.init 8 (fun _ -> string_of_int (int 256))));
  p "struct S { a: u8, w: u16, b: u8 }\nvar s1: S\nvar ss: [S; 4]\n";
  p "lut fn l8(x: u8) -> u8 { return (x * %d) ^ (x >> %d) }\n" (int 30) (int 8);
  p "lut fn l16(x: u8) -> u16 { var w: u16 = x  return w * %d + %d }\n" (int 300) (int 1000);
  for k = 0 to int 3 do
    let name = Printf.sprintf "f%d" k in
    let ps = List.init (int 3) (fun _ -> pick [ "u8"; "u16"; "bool" ]) in
    let ret = pick [ ""; "u8"; "u16" ] in
    let args = List.mapi (fun i t -> Printf.sprintf "p%d" i, t, true) ps in
    p "fn %s(%s)%s {\n" name
      (String.concat ", " (List.map (fun (x, t, _) -> x ^ ": " ^ t) args))
      (if ret = "" then "" else " -> " ^ ret);
    p "  var a: u8 = %d\n  var w: u16 = %d\n  var t: bool = false\n" (int 256) (int 65536);
    let vars = args @ [ "a", "u8", true; "w", "u16", true; "t", "bool", true ] @ globals in
    block "  " vars 2 false (Some ret);
    if ret <> "" then p "  return %s\n" (num ret vars 2);
    p "}\n";
    fns := (name, ps, ret) :: !fns
  done;
  p "fn main() {\n  var a: u8 = %d\n  var w: u16 = %d\n  var t: bool = true\n" (int 256) (int 65536);
  block "  " ([ "a", "u8", true; "w", "u16", true; "t", "bool", true ] @ globals) 3 false None;
  p "  puth(a) puth(w) puth(t as u8) puth(g8) puth(g16) puth(gb as u8)\n";
  p "  for i in 0..8 { puth(mem16[0xc000 + i * 32]) puth(arr[i]) }\n";
  p "  puth(s1.a) puth(s1.w) puth(s1.b)\n  for i in 0..4 { var e = ss[i]  puth(e.a) puth(e.w) puth(e.b) }\n}\n";
  Buffer.contents b

(* 乱数で作る match。値の並び（続き番号、とびとび、同じ行き先）をいろいろにして、0〜255 を全部流す。 *)
let make_match seed =
  let st = Random.State.make [| seed |] in
  let int n = Random.State.int st n in
  let b = Buffer.create 256 in
  let p fmt = Printf.bprintf b fmt in
  p "fn main() {\n  var x: u8 = 0\n  var n: u16 = 0\n  loop {\n    match x {\n";
  let v = ref (int 3 * int 100) in
  for i = 1 to 1 + int 14 do
    if !v < 256 then begin
      let k = min (1 + int 3) (256 - !v) in
      p "      %s => { n += %d }\n" (String.concat ", " (List.init k (fun j -> string_of_int (!v + j)))) (i * 7);
      v := !v + k + (if int 2 = 0 then 0 else int (1 lsl int 7))
    end
  done;
  p "      _ => { n += %s }\n    }\n    x += 1\n    if x == 0 { break }\n  }\n  puth(n)\n}\n" (if int 2 = 0 then "x" else "1");
  Buffer.contents b

(* 乱数で作るタスク。1 個だけのタスクとプールのタスクを作り、main から何度か進めて、途中の出力と
   最後の状態を比べる。中断をまたぐ変数、引数、wait、match の中の yield、個体を外から見る・止める、を混ぜる。 *)
let make_task seed =
  let st = Random.State.make [| seed |] in
  let int n = Random.State.int st n in
  let pick l = List.nth l (int (List.length l)) in
  let b = Buffer.create 1024 in
  let p fmt = Printf.bprintf b fmt in
  (* タスクの本体に置く文。vars8 / vars16 はそこで使える変数 *)
  let rec steps ind n v8 v16 =
    for _ = 1 to n do
      let x = pick v8 and w = pick v16 in
      match int 12 with
      | 0 -> p "%s%s += %d\n" ind x (1 + int 5)
      | 1 -> p "%s%s += %s\n" ind w x
      | 2 -> p "%sg += %s\n" ind w
      | 3 -> p "%sputc('a' + (%s & 15))\n" ind x
      | 4 -> p "%sputh(%s)\n" ind w
      | 5 | 6 -> p "%syield\n" ind
      | 7 -> p "%swait %s & 3\n" ind x
      | 8 ->
        p "%sif (%s & 1) == 0 {\n" ind x;
        steps (ind ^ "  ") 2 v8 v16;
        p "%s} else {\n" ind;
        steps (ind ^ "  ") 1 v8 v16;
        p "%s}\n" ind
      | 9 ->
        p "%smatch %s & 3 {\n%s  0 => {\n" ind x ind;
        steps (ind ^ "    ") 2 v8 v16;
        p "%s  }\n%s  1, 2 => { %s -= 1 }\n%s  _ => { yield }\n%s}\n" ind ind w ind ind
      | 10 -> p "%sg8 = g8 + %s\n" ind x
      | _ -> p "%s%s = %s ^ %d\n" ind x x (int 256)
    done
  in
  p "var g: u16\nvar g8: u8\n";
  let pool = 2 + int 3 in
  p "task unit(x: u8, w: u16) pool %d {\n  var k: u8 = %d\n  var acc: u16 = %d\n" pool (2 + int 4) (int 1000);
  p "  while k != 0 {\n";
  steps "    " (3 + int 4) [ "x"; "k" ] [ "w"; "acc" ];
  p "    k -= 1\n    yield\n  }\n  puth(acc)\n}\n";
  p "task solo() {\n  var n: u8 = %d\n  var t: u16 = %d\n  loop {\n" (3 + int 5) (int 500);
  steps "    " (3 + int 4) [ "n" ] [ "t" ];
  p "    n -= 1\n    if n == 0 { return }\n    yield\n  }\n}\n";
  p "fn main() {\n";
  for _ = 1 to 1 + int (pool + 1) do p "  spawn unit(%d, %d)\n" (int 256) (int 65536) done;
  p "  spawn solo()\n  for i in 0..%d {\n    resume unit\n    resume solo\n" (4 + int 8);
  (match int 4 with
   | 0 -> p "    for e in unit { g += e.x  if e.x > %d { stop e } }\n" (100 + int 150)
   | 1 -> p "    for e in unit { e.w += 1  g8 += e.x }\n    if i == 3 { spawn unit(i, 7) }\n"
   | 2 -> p "    if i == %d { stop solo  spawn unit(g8, g) }\n" (int 4)
   | _ -> ());
  p "    putc('|')\n  }\n";
  p "  puth(g) puth(g8) puth(running(unit) as u8) puth(running(solo) as u8)\n}\n";
  Buffer.contents b

(* 乱数で作るスタック操作。添字 sp は st を引くことと足し引きにしか使わないので、番地に直される（Ptr）。
   向きの選び直しと、動かす順番の入れ替えを通る。深さは作るときに数えて、表の外へ出ないようにする。 *)
let make_stack seed =
  let st = Random.State.make [| seed |] in
  let int n = Random.State.int st n in
  let pick l = List.nth l (int (List.length l)) in
  let b = Buffer.create 512 in
  let p fmt = Printf.bprintf b fmt in
  let e () = pick [ string_of_int (int 256); "acc"; "acc + 1"; "st[sp - 1]" ] in
  let ops ind depth n =
    let d = ref depth in
    for _ = 1 to n do
      match int 9 with
      | 0 | 1 when !d < 10 -> p "%sst[sp] = %s  sp += 1\n" ind (if !d > 0 then e () else string_of_int (int 256)); incr d
      | 2 when !d < 9 -> p "%sst[sp] = %d  st[sp + 1] = acc  sp += 2\n" ind (int 256); d := !d + 2
      | 3 when !d > 0 -> p "%ssp -= 1  acc += st[sp]\n" ind; decr d
      | 4 when !d >= 2 -> p "%ssp -= 1  st[sp - 1] = st[sp - 1] %s st[sp]\n" ind (pick [ "+"; "-"; "^" ]); decr d
      | 5 when !d >= 1 && !d < 10 -> p "%sst[sp] = st[sp - 1]  sp += 1\n" ind; incr d
      | 6 when !d >= 2 -> p "%sacc2 = st[sp - 1]  st[sp - 1] = st[sp - 2]  st[sp - 2] = acc2\n" ind
      | 7 when !d >= 2 -> p "%sst[sp - 2] ^= %d  acc += st[sp - 1]\n" ind (int 256)
      | 8 when !d >= 2 -> p "%ssp -= 2  puth(st[sp] + st[sp + 1])\n" ind; d := !d - 2
      | _ -> p "%sacc += %d\n" ind (int 7)
    done;
    !d
  in
  p "var st: [u8; 16]\nfn main() {\n  var sp: u8 = 0\n  var acc: u8 = %d\n  var acc2: u8 = 0\n" (int 256);
  let d = ops "  " 0 (4 + int 6) in
  p "  for i in 0..%d {\n" (2 + int 3);
  let d' = ops "    " d (3 + int 5) in
  (* ループの中は、深さを元に戻して終わる *)
  for _ = d' + 1 to d do p "    st[sp] = acc  sp += 1\n" done;
  for _ = d + 1 to d' do p "    sp -= 1  acc ^= st[sp]\n" done;
  p "  }\n";
  let d = ops "  " d (int 5) in
  for _ = 1 to d do p "  sp -= 1  puth(st[sp])\n" done;
  p "  puth(acc)\n}\n";
  Buffer.contents b

let () =
  let fails = ref 0 in
  List.iter
    (fun (name, src, want) ->
      match both src with
      | Out got when got = want -> ()
      | Out got -> incr fails; Printf.printf "NG %s\n  期待: %S\n  結果: %S\n" name want got
      | Skip m | Fail m -> incr fails; Printf.printf "NG %s: %s\n" name m)
    cases;
  List.iter
    (fun (name, src, want) ->
      let has m =
        let n = String.length want in
        let rec go i = i + n <= String.length m && (String.sub m i n = want || go (i + 1)) in
        go 0
      in
      match compile (prelude ^ src) with
      | exception Ast.Error (_, m) when has m -> ()
      | exception Ast.Error (_, m) -> incr fails; Printf.printf "NG %s: 別のエラー: %s\n" name m
      | _ -> incr fails; Printf.printf "NG %s: エラーにならなかった\n" name)
    errors;
  (* match の出し方を 1 つに絞っても、結果は同じはず。その出し方では出せない例は飛ばす。 *)
  List.iter
    (fun way ->
      Gen.only := Some way;
      List.iter
        (fun (name, src, want) ->
          match both src with
          | Out got when got = want -> ()
          | Out got -> incr fails; Printf.printf "NG %s（%s）\n  期待: %S\n  結果: %S\n" name way want got
          | Skip _ -> ()
          | Fail m -> incr fails; Printf.printf "NG %s（%s）: %s\n" name way m)
        cases)
    [ "lin"; "bin"; "tab" ];
  Gen.only := None;
  for seed = 1 to 60 do
    Gen.only := List.nth [ None; Some "lin"; Some "bin"; Some "tab" ] (seed mod 4);
    match both (make_match seed) with
    | Out _ | Skip _ -> ()
    | Fail m -> incr fails; Printf.printf "NG match %d: %s\n%s\n" seed m (make_match seed)
  done;
  Gen.only := None;
  for seed = 1 to 80 do
    match both (make_stack seed) with
    | Out _ | Skip _ -> ()
    | Fail m -> incr fails; if !fails <= 3 then Printf.printf "NG スタック %d: %s\n%s\n" seed m (make_stack seed)
  done;
  let tasks_ok = ref 0 in
  for seed = 1 to 60 do
    match both (make_task seed) with
    | Out _ -> incr tasks_ok
    | Skip _ -> ()
    | Fail m -> incr fails; if !fails <= 3 then Printf.printf "NG タスク %d: %s\n%s\n" seed m (make_task seed)
  done;
  Printf.printf "lang: 乱数のタスク %d 件一致\n" !tasks_ok;
  let n = try int_of_string (Sys.getenv "HL_RANDOM") with Not_found -> 100 in
  let ok = ref 0 and skip = ref 0 in
  for seed = 1 to n do
    let src = make seed in
    let t0 = Sys.time () in
    let r = both src in
    (* HL_SLOW=秒 を付けると、それより時間のかかったプログラムの番号を出す *)
    (match Sys.getenv_opt "HL_SLOW" with
     | Some lim when Sys.time () -. t0 > float_of_string lim ->
       Printf.printf "遅い: 乱数 %d は %.1f 秒（%d 行）\n%!" seed (Sys.time () -. t0)
         (List.length (String.split_on_char '\n' src))
     | _ -> ());
    match r with
    | Out _ -> incr ok
    | Skip _ -> incr skip
    | Fail m ->
      incr fails;
      if !fails <= 3 then Printf.printf "NG 乱数 %d: %s\n%s\n" seed m src
  done;
  if Sys.getenv_opt "HL_STATS" <> None then
    Printf.printf "探索: %d 回試して %d 回行き詰まり\n" !Alloc.trials !Alloc.stuck;
  Printf.printf "lang: 例 %d 件、乱数 %d 件一致（%d 件は長すぎて省略）\n" (List.length cases) !ok !skip;
  if !fails > 0 then (Printf.printf "失敗 %d 件\n" !fails; exit 1)
