# 8. MSX で動かす：画面と音

VDP（画面）と PSG（音）を動かすための部品と、MSX 用のライブラリです。

## 例

[examples/music.hl](../examples/music.hl) は、曲を鳴らしながら画面に文字を流します。曲はバイトコードで書き、タスクがそれを読みます。

```text
import "msx.hl"

enum Cmd { NOTE, REST, AGAIN }
data pitch: u16 = [ 428, 381, 339, 320, 285, 254, 226, 214 ]      // ドレミファソラシド
data song: u8 = [ NOTE, 0, 12,  NOTE, 2, 12,  NOTE, 4, 12,  REST, 6,  AGAIN ]

task music() {
  var p: u16 = song
  loop {
    match next(p) as Cmd {
      NOTE => { tone(0, pitch[next(p)])  volume(0, 12)  wait next(p) }
      REST => { volume(0, 0)  wait next(p) }
      AGAIN => { p = song }
    }
  }
}

fn tick() { resume music }                   // 画面の割り込みごとに呼ばれる

fn main() {
  chgmod(1)
  gicini()
  spawn music()
  on_vsync(&tick)
  loop { vsync()  ... }
}
```

```sh
dune exec bin/hlc.exe -- examples/music.hl       # music.rom（569 バイト）
openmsx -machine C-BIOS_MSX1 -cart examples/music.rom
```

openMSX で、PSG の周期と音量が曲のとおりに変わり、同時に文字が流れることを確かめてあります。

[examples/tracks.hl](../examples/tracks.hl) は、同じタスクを 3 個動かして（プール、7 章）、3 つのチャンネルを別々の曲データで鳴らします。

[examples/game.hl](../examples/game.hl) は小さなシューティングです。カーソルキーの左右で動き、スペースで撃ちます。敵（4 匹まで）と弾（3 発まで）はプールのタスク、「敵を出す係」は 1 個だけのタスク、曲は割り込みで進む 2 トラックです。敵は符号付きの速度で左右に揺れ、当たり判定は `for b in shot { for e in zako { ... } }` で全部の組を突き合わせます。ROM は 1486 バイトです。openMSX でキーを押したことにして、敵が揺れながら降りる、弾が 3 発飛ぶ、当たると両方が消えて点が入る、自機が動く、曲が鳴り続ける、を確かめてあります。

`examples/` には Makefile があります。

```sh
cd examples
make game          # game.hl を ROM にして openMSX で動かす（tracks, music, hello なども同じ）
make tracks.rom    # ROM を作るだけ
make clean
```

## 言語の部品

| 書き方 | 意味 |
| --- | --- |
| `port[n] = v`、`port[n]` | I/O ポートの読み書き。`n` は定数 |
| `out(port, v)`、`inp(port)` | ポート番号を変数で選ぶ読み書き |
| `outs(port, src, n)` | メモリの n バイトを続けてポートへ送る（`otir`）。VRAM へまとめて書くのに使う |
| `copy(dst, src, n)` | メモリを写す（`ldir`） |
| `extern fn 名前(引数: 型 @ レジスタ) @ 番地` | BIOS など、決まった番地のルーチンを呼ぶ（3 章） |
| `&名前` | 関数（引数なし）や表の番地。フックに登録するのに使う |
| `ei()` `di()` `halt()` | その 1 命令になる。割り込みの許可・禁止と、割り込み待ち |
| `import "ファイル"` | 別のファイルを取り込む。同じファイルは 1 回だけ |

`import` は、元のファイルの隣、その隣の `lib/`、1 つ上の `lib/`、環境変数 `HLANG_LIB` の順に探します。

**使わなかった関数は ROM に入りません。** `main` から呼び出しと `&名前` をたどって、届く関数だけを出します。ライブラリを丸ごと `import` しても、大きさは使った分だけです。

## ライブラリ

[lib/msx.hl](../lib/msx.hl) にあります。

| 分類 | 関数 |
| --- | --- |
| 文字 | `chput(c)` `print(s)` `locate(x, y)` `chgmod(mode)` `chgclr()` |
| 数 | `print_u8(v)` `print_u16(v)` `print_i8(v)` `print_i16(v)`（10 進。負の数は `-` を付ける） |
| VDP | `wrtvdp(v, r)` `rdvrm(addr)` `wrtvrm(addr, v)` `filvrm(addr, n, v)` `ldirvm(src, dst, n)` `vram_at(addr)` |
| PSG | `gicini()` `wrtpsg(r, v)` `rdpsg(r)` `tone(ch, period)` `volume(ch, v)` |
| スプライト | `sprite(n, x, y, pat, color)` `sprite_pattern(pat, &表)` |
| 入力 | `gtstck(n)` `gttrig(n)` `snsmat(row)` |
| 時間 | `vsync()` `on_vsync(&f)` |

`vram_at(addr)` で書き込み位置を決めた後は、`port[VDP_DATA] = v` と書くたびに 1 バイトずつ進みます。

## 割り込みで呼ばれる関数

`on_vsync(&tick)` は、BIOS のフック（H.TIMI）に `jp tick` を書きます。BIOS がレジスタを保存してから呼ぶので、`tick` は普通の関数で構いません。

気を付けることが 1 つあります。hlang の変数は固定の番地に置かれます（再帰がないので、スタックフレームを作りません）。**割り込みから呼ばれる関数と、普段の処理から呼ばれる関数は、分けてください。** 同じ関数を両方から呼ぶと、普段の処理が使っている最中の変数を、割り込みが書き換えます。上の例では、`tone` と `volume` を呼ぶのは割り込み側の `music` だけです。

コンパイラがこれを検査します。`&名前` で番地を取った関数は割り込みから呼ばれるとみなし、そこから届く関数やタスクを `main` からも呼んでいると、エラーになります。

```text
beep は割り込みから（&tick 経由で）も、普段の処理（main）からも呼ばれる。変数が壊れるので、どちらか専用にする
```

BIOS のルーチン（`extern`）は検査しません。VDP のポートを割り込みと普段の処理の両方から触る場合は、自分で `di()` `ei()` で囲んでください。

## まだないもの

- SMS などの他の機種。
- 16x16 のスプライトや、スプライトの重なり（1 行に 5 枚以上）への対処。
