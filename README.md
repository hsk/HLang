# hlang

hlang は、Z80 のマシンコードを出力する小さなコンパイル言語です。コンパイラは OCaml で書かれています。MSX（ROM カートリッジ）とセガ・マスターシステム（SMS）向けの ROM を作れます。

## 目指すもの

- **HL を軸にした速いコード**：遅い IX/IY ではなく、HL でメモリを歩きます。変数の置き場所と呼び出し規約は、実際にコードを出して測りながら関数ごとに決まります。
- **待ちや再開を順番どおりに書けること**：`wait` と `yield` を使うと、「敵を出す、少し待つ、次を出す」を、状態番号と分岐を手で管理せずに書けます。
- **インタプリタを綺麗に速く書けること**：音楽ドライバのバイトコードマシンやゲーム進行のスクリプトを、列挙型と網羅性チェック付きの `match` で書けます。`match` は、順に比べる・2 分探索・表で飛ぶ、のうち安いものが選ばれます。
- **出力が読めること**：生成されるアセンブリは、継続（引数を持つラベル）ごとのブロックで、各ブロックの先頭に値の置き場所がコメントで付きます。

ヒープも GC も再帰もありません。変数は固定の番地に置かれるので、スタックフレームは要らず、タスクが使うメモリの大きさもコンパイル時に分かります。

## 書き味

```text
import "msx.hl"

data lane: u8 = [ 40, 200, 120, 80 ]

task zako(x: u8, y: u8) pool 4 {          // 同時に 4 匹まで
  while y < 176 {
    sprite(4, x, y, 1, 9)
    y += 1
    yield                                  // 次のフレームまで
  }
}

task wave() {
  var i: u8 = 0
  loop {
    spawn zako(lane[i & 3], 0)
    i += 1
    wait 48                                // 48 フレーム待つ
  }
}

fn main() {
  chgmod(1)
  spawn wave()
  loop {
    vsync()
    resume zako                            // 動いている全部を 1 歩進める
    resume wave
  }
}
```

主な部品：

| 分類 | 内容 |
| --- | --- |
| 型 | `u8` `u16` `i8` `i16` `bool`、列挙型、構造体（`*名前` で番地として扱う）、表と配列 |
| 文 | `if` `while` `loop` `for i in a..b` `match` `break` `continue` `return` |
| タスク | `task` `yield` `wait` `spawn` `resume` `running` `stop`、`pool n`、`for e in タスク` |
| 速度のため | `lut fn`（コンパイル時に表にする関数）、`inline fn`（呼ぶ所に展開。とても小さい関数は自動）、`next(p)`（読んで進める）、`copy` `outs`（`ldir` `otir`）。1 バイトずつのコピー、塗りつぶし、ポートへの読み書きのループは、自動で `ldir` `lddr` `otir` `inir` などにする（[docs/05-codegen.md](docs/05-codegen.md)） |
| 機種とのつなぎ | `extern fn`（BIOS の呼び出し）、`port[n]`、`&関数名`、`ei` `di` `halt`、`import` |

言語の詳しい説明は [docs/03-language.md](docs/03-language.md)、タスクは [docs/07-tasks.md](docs/07-tasks.md) にあります。

## 使い方

OCaml 5 以上と dune が要ります。Windows では、WSL2 の中（[docs/wsl.md](docs/wsl.md)）でも、ネイティブの PowerShell（[docs/windows.md](docs/windows.md)）でも動かせます。

```sh
dune build
dune exec bin/hlc.exe -- examples/hello.hl         # hello.rom（MSX の 16KB ROM）
dune exec bin/hlc.exe -- examples/sms_game.hl -sms # sms_game.sms（SMS の 32KB ROM）
dune exec bin/hlc.exe -- examples/sum.hl --asm     # アセンブリを見る
dune exec bin/hlc.exe -- examples/hello.hl --run   # 内蔵のエミュレータで走らせる
dune exec bin/hlc.exe -- examples/game.hl --report # 関数ごとの大きさ、呼び出し規約、時間の上限、RAM
dune test                                          # 全テスト
```

`-O0` を付けると最適化なし（変数をすべてメモリに置く）になります。コンパイルは一瞬です。

例は [examples/](examples/) にあります。`cd examples && make game` で openMSX が、`make sms` で MAME が起動します。

| ファイル | 内容 |
| --- | --- |
| [hello.hl](examples/hello.hl) | 文字を出す |
| [script.hl](examples/script.hl) `vm.hl` | バイトコードマシン |
| [music.hl](examples/music.hl) `tracks.hl` | 曲のバイトコードをタスクが読んで、PSG を鳴らす |
| [game.hl](examples/game.hl) | 小さなシューティング（MSX）。敵と弾のプール、当たり判定、スプライト、割り込みの曲 |
| [sms_game.hl](examples/sms_game.hl) | 同じゲームの SMS 版 |

ライブラリは [lib/msx.hl](lib/msx.hl) と [lib/sms.hl](lib/sms.hl) です。使わなかった関数は ROM に入りません。

## どのくらい速いか

小さなループ 4 つの比較では、SDCC 4.6（`--opt-code-speed`）の 159 バイト・71,360 T に対して、hlang は 107 バイト・49,297 T です（[bench/run.sh](bench/run.sh)）。8 命令のバイトコードマシンは、`stack[sp]` と素直に書いても 142,386 T で、手書きアセンブリの 146,348 T より速く動きます（[bench/vm.sh](bench/vm.sh)）。比較の量はまだ少なく、最適とは主張しません。

ゲーム 1 本の比較には、作例 [hsk/damaxian_hl](https://github.com/hsk/damaxian_hl) があります。CoBinee さんの DAMAXIAN（MSX）を、C 版から hlang に移したもので、元のアセンブラ版・C（SDCC）版と同じ動きで比べられます。今の版（main）では、タイマ割り込みの処理 1 回が平均 43,739 T で、手書きのアセンブラ版（46,190 T）より約 5% 速く、SDCC 版（69,037 T）より 3 割以上速くなりました（同じ測り方。1/60 秒は 59,659 T）。

## 検証

- エミュレータは zexdoc・zexall の全 67 項目が通ります。アセンブラは全 697 命令で `z80asm` と一致します。
- 同じプログラムを、参照インタプリタと、コンパイルした機械語（エミュレータ上）の両方で実行して、出力を突き合わせます。プログラムは乱数でも作ります。
- ゲームは openMSX と MAME でも動かして確かめています。

## 文書

- [docs/making.md](docs/making.md)：コンパイラの作り方（章立ての入口）
- [docs/wsl.md](docs/wsl.md)：Windows（WSL）で使う
- [docs/windows.md](docs/windows.md)：Windows のネイティブ（PowerShell）で使う
- [docs/09-zma.md](docs/09-zma.md)：ZMA（HRA! さんのアセンブラ）の書き方を読む・書く
- [plan.md](plan.md)：計画と進み具合、残っていること
- [concept.md](concept.md)：構想と、先行する実装の調査記録

## ライセンス

MIT ライセンスです（[LICENSE](LICENSE)）。コンパイラ、`lib/` のライブラリ、`examples/`、文書のすべてが対象です。hlang で作った ROM には、`lib/` から取り込んだコードが入りますが、MIT なので、ROM を配るときに特別な手続きは要りません（著作権の表示は LICENSE のとおり）。
