# 7. タスク：途中で止まって、続きから動く

「音を鳴らす、12 フレーム待つ、次の音を鳴らす」を、状態番号と分岐を手で管理せずに、順番どおりに書くための仕組みです。

## 例

```text
task count() {
  for i in 0..3 { chput('0' + i)  yield }
  chput('!')
}

fn main() {
  spawn count()                              // 最初から始める状態にする
  while running(count) { resume count  chput('|') }
}
```

出力は `0|1|2|!|` です。`resume` のたびに、タスクは次の `yield` まで進んで戻ってきます。

## 書き方

| 書き方 | 意味 |
| --- | --- |
| `task 名前() { ... }` | タスクを定義する。1 個だけ動く |
| `task 名前(x: u8) pool 8 { ... }` | 同時に 8 個まで動かせるタスク（プール）。引数を取れる |
| `yield` | 中断して、`resume` した側へ戻る。タスクの本体でだけ書ける |
| `wait n` | `n` 回 `yield` する（`n` は u8）。`n` フレーム待つのに使う |
| `spawn 名前()` | タスクを「最初から始める」状態にする。動いている途中でも最初に戻る |
| `spawn 名前(引数)` | プールなら、空いている枠で 1 個始める。空きがなければ何もしない |
| `resume 名前` | タスクを次の `yield` まで進める。終わったタスクや、まだ `spawn` していないタスクなら何もしない。プールなら、動いている全部を 1 歩ずつ進める |
| `running(名前)` | まだ終わっていなければ真。プールなら、1 個でも動いていれば真 |
| `return` | タスクを終える |
| `stop 名前` | タスクを止める。プールなら全部の個体を止める |
| `for e in 名前 { ... }` | プールの、動いている個体を順に見る。`e` は個体を指す値（型は `*名前`） |
| `e.引数名` | 個体の引数を、外から読み書きする |
| `stop e` | その個体を止める。枠は空きになる |

`yield` と `wait` は、タスクの本体に直接書きます。タスクから呼んだ関数の中では中断できません。

## 出力

上の `count` は次のコードになります。

```asm
; task count()
count:
    xor a
    ld (count.i.5),a
    ld a,(count.i.5)
    cp 3
    jr nc,count.done.2
; loop loop.1(i.4: mem)
count.loop.1.body:
    ld a,0x30
    ld hl,count.i.5
    add a,(hl)
    call 0xa2
    ld hl,count.resume2          ; 続きの番地を覚えて
    ld (count.pc),hl
    ret                          ; resume した側へ戻る
; resume
count.resume2:                   ; 次の resume でここから
    ld hl,count.i.5
    inc (hl)
    ld a,(count.i.5)
    cp 3
    jp c,count.loop.1.body
; cont done.2(i.5: mem)
count.done.2:
    ld a,0x21
    call 0xa2
count.done:                      ; 終わったタスク。resume されても何もせずに戻る
    ld hl,count.done
    ld (count.pc),hl
    ret
count.resume:                    ; resume count はここを call する
    ld hl,(count.pc)
    jp (hl)
```

状態番号も、状態での分岐もありません。タスクが持つのは「続きの番地」2 バイト（`count.pc`）と、中断をまたいで使う変数だけです。`spawn count()` は `count.pc` に本体の先頭の番地を入れるだけ、`running(count)` は `count.pc` が `count.done` を指しているかを見るだけです。

## 作り

中間表現は継続でできているので（4 章）、「この先の続き」はもともと名前の付いた場所です。`yield` は、その場所の番地を保存して戻るだけで済みます。足したのは次のものです。

| 場所 | 足したもの |
| --- | --- |
| 中間表現（[src/ir.ml](../src/ir.ml)） | `Yield 続き` |
| 型検査（[src/check.ml](../src/check.ml)） | `wait n` を「回数を変数に取って、0 になるまで `yield`」に直す |
| CPS 変換（[src/cps.ml](../src/cps.ml)） | `spawn` は番地の書き込み、`resume` は `名前.resume` の呼び出しに直す |
| コード生成（[src/gen.ml](../src/gen.ml)） | `Yield`：続きの番地を保存して `ret`、ラベルを置いて続きを出す |

### 中断をまたぐ変数はメモリに置く

`ret` で戻った後、レジスタの中身は残りません。そこでコード生成は、`Yield` の地点で生きている変数がレジスタに置かれていたら「行き詰まり」にします。置き場所の探索（5 章）は行き詰まった割り当てを捨てるので、中断をまたぐ変数は自然にメモリに落ち着きます。保存と復元のコードは要りません。

またがない変数は、今までどおりレジスタに乗ります。上の例の `i` はメモリですが、`'0' + i` の計算結果は A に乗ったままです。

### タスクの終わり

タスクの `return` は、ただの `ret` ではなく `名前.done` へ飛びます。そこで続きの番地を自分自身に向けてから戻ります。条件付きの `ret` や末尾呼び出し（`call` を `jp` にする）は、この印を付け損なうので、タスクの本体では使いません。

### 参照インタプリタ

参照インタプリタ（[src/interp.ml](../src/interp.ml)）は、OCaml のエフェクトで同じ動きをします。`yield` で「ここから先」を取っておき、`resume` でその続きを走らせます。機械語との突き合わせは、タスクでも同じように回ります。

## プール：同じタスクを何個も動かす

```text
task zako(ch: u8, n: u8) pool 3 {
  var i: u8 = 0
  while i != n { chput(ch + i)  i += 1  yield }
  chput('.')
}

fn main() {
  spawn zako('a', 2)
  spawn zako('A', 3)
  while running(zako) { resume zako  chput('|') }
}
```

出力は `aA|bB|.C|.|` です。2 個の `zako` が、それぞれの `ch` `n` `i` を持って交互に進みます。

### フレーム

個体ごとに「フレーム」を持ちます。中身は、続きの番地 2 バイトと、中断をまたいで使う変数です。

```text
zako.pool:   [続きの番地][ch][n][i]   [続きの番地][ch][n][i]   [続きの番地][ch][n][i]   [予備]
                         ↑ 動かすとき、HL がここを指して届く
```

- 引数はフレームの先頭に並びます。`spawn` は空いている枠を探し、そこへ引数を書きます。
- 続きの番地の上位バイトが 0 なら、その枠は空きです。RAM は起動時に 0 で埋めるので、最初は全部空きです。
- 空きがないとき、`spawn` は「予備の枠」を返します。引数はそこへ書かれて捨てられるので、呼ぶ側に「空きがあったか」の分岐が要りません。
- フレームに置くのは、**宣言より後ろの同じブロックに `yield` か `wait` がある変数**だけです。中断をまたがない変数は、普通の変数としてレジスタに乗ります。

### フレームの読み書き：HL で歩くか、IX を使うか

動かすとき、フレームの番地は HL で届きます（変数 `self`）。この `self` の置き場所を、他の変数と同じように探索で決めます。候補は HL、IX、DE、BC、メモリです。

**HL に置けるなら、HL を進めながら読み書きします。** 上の `zako` はこうなります。

```asm
; task zako(self.1: HL)
zako.loop.3:
    inc hl
    inc hl
    ld c,(hl)                    ; i
    dec hl
    ld a,(hl)                    ; n
    cp c
    dec hl
    jr z,zako.done.4
    ld a,(hl)                    ; ch
    add a,c
    call 0xa2
    inc hl
    inc hl
    ld a,(hl)                    ; i += 1
    inc a
    ld (hl),a
    dec hl
    dec hl
    dec hl
    ld (hl),(zako.resume1)>>8    ; 続きの番地は、変数の手前 2 バイト
    dec hl
    ld (hl),(zako.resume1)&255
    ret
```

**HL が他のことに要るなら、IX になります。** HL を壊す関数を呼ぶ、16bit の計算をする、表を引く、といった場合です。`self` はタスクの間ずっと生きているので、HL に置いたままではそれらの命令が出せず、その割り当ては「行き詰まり」になります。探索は次の候補の IX を試します。[examples/tracks.hl](../examples/tracks.hl) のトラックがそうです。

```asm
; task track(self.34: IX)
track:
    push hl
    pop ix                       ; 届いた HL を IX に移す
    ...
track.loop.38:
    ld l,(ix+3)                  ; p
    ld h,(ix+4)
    ld a,(hl)                    ; next(p)
    inc hl
    ld (ix+3),l
    ld (ix+4),h
```

どちらにするかを書く必要はありません。同じソースのまま、出して測って安い方になります。`(ix+d)` の読み書きは 19 T、`(hl)` は 7 T ですが、HL を何度も進めたり戻したりするなら IX の方が安いこともあり、それも値段で決まります。

外の関数（`extern`）は IX を壊さないものとして扱います。MSX の BIOS では、スロットをまたぐ呼び出し（CALSLT など）が IX と IY を使うので、プールのタスクからは呼ばないでください。

### 動かすルーチン

プールごとに、次のルーチンが出ます（[src/alloc.ml](../src/alloc.ml) の `pool_code`）。呼ばれていないものは出しません。

| ルーチン | 中身 |
| --- | --- |
| `名前.resume` | フレームを順に見て、空きでなければ続きの番地へ飛ぶ。HL はフレームの変数を指す |
| `名前.spawn` | 空きを探し、続きの番地を本体の先頭にして、HL で変数の場所を返す |
| `名前.live` | 動いている個数を A で返す |

### 型検査がやること

プールのタスクの中では、フレームに置く変数を `mem[self + 位置]` に書き換えます（[src/check.ml](../src/check.ml) の `field` と `var`）。構造体のフィールド `p.f` と同じ形なので、コード生成は構造体と同じ仕組み（HL を進める、または `(ix+d)`）で読み書きします。中間表現より後ろに、プール専用の変数の扱いはありません。

### 個体を外から見る

プールのタスクは、引数をフィールドに持つ構造体としても見えます。引数が、外から見える部分です。

```text
task shot(n: u8, x: u8, y: u8) pool 3 { ... }
task zako(n: u8, x: u8, y: u8, dx: i8) pool 4 { ... }

fn collide() {
  for b in shot {
    for e in zako {
      if b.x < e.x + 8 && e.x < b.x + 8 && b.y < e.y + 8 && e.y < b.y + 8 {
        score += 1
        stop b
        stop e
        break
      }
    }
  }
}
```

`for e in zako` は、フレームを先頭から順に見て、空きでないものだけ本体を実行します。`e.x` は構造体のフィールドと同じ読み書きです。`stop e` は、続きの番地の上位バイトを 0 にするだけです。タスクの本体の中で宣言した変数は、外からは見えません。

`for ... in` の中では `yield` できません。動いている最中の個体が自分を `stop` しても、次の `yield` でまた動いている状態に戻ります（自分で終わるには `return` を使います）。

## 制限

- プールのタスクでは、`yield` を含む `for` は書けません（`while` で書きます）。
- フレームは 120 バイトまで、個数は 255 個までです。
- `spawn` は、始めた個体を指す値を返しません。個体は `for ... in` で探します。
- 中断をまたぐ変数はメモリに置かれるので、`yield` を含むループの変数はレジスタより遅くなります。
- タスクの変数は固定の番地にあります。割り込みから `resume` するタスクと、普段の処理から `resume` するタスクが、同じ関数を呼ばないようにしてください（8 章）。
