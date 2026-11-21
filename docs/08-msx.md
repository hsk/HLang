# 8. MSX と SMS で動かす：画面と音

VDP（画面）と PSG（音）を動かすための部品と、MSX 用・セガ・マスターシステム（SMS）用のライブラリです。前半が MSX、最後の節が SMS です。

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

[examples/game.hl](../examples/game.hl) は小さなシューティングです。カーソルキーの左右で動き、スペースで撃ちます。敵（4 匹まで）と弾（3 発まで）はプールのタスク、「敵を出す係」は 1 個だけのタスク、曲は割り込みで進む 2 トラックです。敵は符号付きの速度で左右に揺れ、当たり判定は `for b in shot { for e in zako { ... } }` で全部の組を突き合わせます。ROM は 1477 バイトです。openMSX でキーを押したことにして、敵が揺れながら降りる、弾が 3 発飛ぶ、当たると両方が消えて点が入る、自機が動く、曲が鳴り続ける、を確かめてあります。

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

## セガ・マスターシステム

```sh
dune exec bin/hlc.exe -- examples/sms_game.hl -sms     # sms_game.sms（32KB の ROM）
tools/sms-screen.sh examples/sms_game.sms              # MAME で起動して、画面を PNG に撮る（画面は開かない）
cd examples && make sms                                # MAME のウィンドウで遊ぶ
```

`-sms` を付けると、SMS の ROM を作ります。[examples/sms_game.hl](../examples/sms_game.hl) は、上のシューティングの SMS 版です。タスク、プール、当たり判定、曲のバイトコードは MSX 版と同じソースで、違うのは絵と色の登録、入力、点数の出し方だけです。

### MSX との違い

| | MSX | SMS |
| --- | --- | --- |
| プログラムの番地 | 0x4000 から（カートリッジ） | 0 から |
| RAM | 0xe000 から | 0xc000 から |
| BIOS | ある（`extern fn` で呼ぶ） | ない。全部ポートを直接たたく |
| 画面の割り込み | BIOS がフック H.TIMI を呼ぶ | 0x38 番地に自分で入口を置く |
| ROM | 16KB、先頭に `AB` のヘッダ。入らなければ 32KB | 32KB、最後の 16 バイトに `TMR SEGA` とチェックサム |

MSX のプログラムが 16KB に入らないときは、32KB（0x4000〜0xbfff）の ROM にします（`msx32`）。BIOS が開くのは 0x4000〜0x7fff だけなので、ヘッダのすぐ後ろに、自分のスロットを 0x8000〜0xbfff にも出すコードを置いて、そこから始めます。

```asm
4010  call 0x0138                ; RSLREG: 今の基本スロット
      rrca / rrca / and 3        ; 0x4000〜0x7fff のスロット番号
      ...                        ; 拡張スロットなら EXPTBL と SLTTBL から拡張スロット番号を足す
      ld h,0x80
      call 0x0024                ; ENASLT: 0x8000〜0xbfff にも同じスロットを出す
      jp start
```

絵やステージのデータが 32KB にも入らないときは、`hlcc -megarom` でメガロム（ASCII8）を作れます（`msx8`）。ASCII8 は 8KB のバンクを 0x4000、0x6000、0x8000、0xa000 に出し、出すバンクの番号を 0x6000、0x6800、0x7000、0x7800 に書きます。

| 番地 | 中身 |
| --- | --- |
| 0x4000〜0x9fff | プログラム（バンク 0〜2、24KB）。起動コードがバンク 1、2、3 を 0x6000、0x8000、0xa000 に出す |
| 0xa000〜0xbfff | 通常はデータバンク。プログラムが 24KB を超える場合はコードの続き（最大 32KB）を置ける。必要に応じてプログラムが `*(volatile u8 *)0x7800 = 番号;` で切り替える |

データのバンクは、`-bank file` で 8KB ごとに ROM の後ろに足します（通常は初めのファイルが 3 番、32KB のコードを使うときは 4 番）。全体は 2 のべき乗の大きさにそろえます。バンクの中の表は、C では `#define BX ((const i16 *)0xa000)` のように、決まった番地のポインタとして読みます。openMSX では `-romtype ASCII8` を付けて動かします。

機種ごとの違いは、[src/hlang.ml](../src/hlang.ml) の `msx` と `sms`（番地と、先頭に置くコード）と `rom`（ROM の形）にまとまっています。コンパイラの他の部分は機種を知りません。

SMS では 0 番地が起動、0x38 が画面の割り込み、0x66 がポーズボタンと決まっているので、先頭にその 3 つを置きます。

```asm
0000  di
      im 1
      ld sp,0xdff0
      ...                        ; ROM の切り替えを初めの状態に。フックを ret にする
      jp start
0038  push af  ...  push iy      ; レジスタを全部しまう
      in a,(0xbf)                ; VDP の状態を読む（読まないと次の割り込みが来ない）
      call 0xdff8                ; RAM のフック。on_vsync が jp f に書き換える
      pop iy  ...  pop af
      ei
      ret
0066  retn                       ; ポーズボタンは何もしない
```

入口がレジスタをしまうので、`on_vsync(&tick)` に渡す関数は MSX と同じく普通の関数です。「割り込みから呼ばれる関数と普段の処理から呼ばれる関数を分ける」検査も、同じように働きます。

### ライブラリ

[lib/sms.hl](../lib/sms.hl) にあります。BIOS がないので、どれも hlang で書いたポートの読み書きです。

| 分類 | 関数 |
| --- | --- |
| 画面 | `screen()`（モード 4 にして VRAM を空にし、画面を出す）`vdp_reg(r, v)` `vram_at(addr)` `palette(i, rgb)` |
| タイル | `tiles(first, &表, n, color)`（8x8 の 1 色の絵を登録）`put(x, y, tile)`（背景のマスに置く） |
| スプライト | `sprite(n, x, y, pat)` `sprite_pattern(pat, &表, color)` |
| PSG | `tone(ch, period)` `volume(ch, v)`（MSX 版と同じ引数。周期の式も同じ） |
| 入力 | `pad()`（押されているボタンのビットが 1。`PAD_LEFT` `PAD_1` など） |
| 時間 | `vsync()` `on_vsync(&f)` |

MSX と違う所：

- 文字を出す BIOS がないので、数字や文字はタイルとして自分で登録します（例では数字 10 個、80 バイト）。
- スプライトの色は、1 枚ごとではなく絵ごとに決まります。`sprite_pattern` に色を渡し、`sprite` には渡しません。
- 色は `palette` で自分で決めます。0〜15 が背景、16〜31 がスプライトです。

### 確かめ方

- **`dune test`**：[test/t_sms.ml](../test/t_sms.ml) が、`sms_game.hl` を ROM にして、VDP・PSG・パッドを簡単に模したエミュレータで走らせます。HALT で止まるたびに画面の割り込みを入れ、パッドを押したことにして、VRAM の中身（タイル、スプライトの位置、点数）、色、音のレジスタを調べます。敵の下へ動いて撃ち続けると点が入る所まで見ます。
- **MAME**：`tools/sms-screen.sh` で、本物に近いエミュレータ（マーク III）の画面を撮ります。点数の「00」、赤い敵 3 匹、白い自機が映ることを確かめました。

自作のモデルだけでは見つからなかった間違いが 1 つありました。初代の VDP は、使わないレジスタ 3 と 4 の全ビットを 1 にしておかないと、タイルの番地の一部が 0 に固定されます。自作のモデルでは VRAM の中身が正しいので通り、MAME では数字が映りませんでした。「自前のエミュレータは、コード生成と同じ勘違いをすると誤りを見逃す」（計画の検証の節）の実例です。

## まだないもの

- SMS のカートリッジの RAM、ROM の切り替え（32KB を超えるプログラム）、ゲームギア。
- 文字のフォント。数字や文字は、例のように自分でタイルにします。
- 16x16 のスプライトや、スプライトの重なり（1 行に 5 枚以上）への対処。
