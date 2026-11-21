# hlang

hlang は、Z80 向けの新しいコンパイラ基盤です。コンパイラは OCaml で書かれています。MSX（ROM カートリッジ）とセガ・マスターシステム（SMS）向けの ROM を作れます。

高速化の考え方はシンプルで、**HL レジスタをできるだけ使うように、レジスタの使い方を自由にすること**です。変数の置き場所と呼び出し規約を関数ごとに決めることで、HL を軸にした速いコードを出します。まだ新しいコンパイラで、入っている最適化は題材にした DAMAXIAN に特化したものだけです。それでも、その DAMAXIAN では**手書きアセンブラより高速な処理を実現しています**（実測は「どのくらい速いか」にあります。もちろんアセンブラで頑張れば同等、同等以上の処理速度は実現できるでしょう）。

入り口は2つあります。

- **`hlcc`**：C 言語コンパイラです。SDCC で書いたプログラムをコンパイルし直すだけで、より高速に動作させることができます。
- **`hlang`**：独自シンタックスを持った言語です。同様の高速性能を実現しています。タスクや `match` など、ゲーム向きの部品で書きたいときはこちらです。

どちらで書いても、出力は同じコード生成器を通ります。

## C でも速い（hlcc）

`hlcc` は、SDCC の書き方のサブセットを受け付ける C フロントエンドです。

- `#include "msx.hl"` と書くと、hlang のライブラリの関数を C から呼べます。
- SDCC の書き方のインラインアセンブラ（`__asm` … `__endasm`、`__naked`）も使えます。
- 対応範囲の詳しい仕様は [docs/11-c-frontend.md](docs/11-c-frontend.md) にあります。

実例として、DAMAXIAN の C 版（[examples/damaxian_c/](examples/damaxian_c/)）がそのままコンパイルできます。実測は次の節のとおりで、C のソースを変えずに SDCC 版より3割以上速くなります。

## どのくらい速いか

比較の量はまだ少なく、最適とは主張しませんが測れたものは次のとおりです。

**ゲーム1本まるごと（DAMAXIAN）**

CoBinee さんの DAMAXIAN（MSX）を題材に、元のアセンブラ版・C 版と同じ動きで比べられます。数字はタイマ割り込みの処理1回の平均で、同じ測り方です（1/60秒は 59,659 T）。

| 版 | 平均 T | 備考 |
| --- | --- | --- |
| hlang 版 | 42,871 | 手書きアセンブラ版より約7%速い |
| C 版を hlcc でコンパイル | 43,617 | ソースは C のまま |
| 手書きアセンブラ版（原作） | 46,190 | |
| C 版を SDCC でコンパイル | 69,037 | hlcc 版より3割以上遅い |

**小さなループ4つ（対 SDCC）**

SDCC 4.6（`--opt-code-speed`）の 159 バイト・71,360 T に対して、hlang は 99 バイト・49,850 T です（[bench/run.sh](bench/run.sh)）。

**バイトコードマシン（対 手書きアセンブリ）**

8命令のバイトコードマシンは、`stack[sp]` と素直に書いても 137,598 T で、手書きアセンブリの 146,348 T より速く動きます（[bench/vm.sh](bench/vm.sh)）。

## 独自文法もある（hlang 言語）

C の代わりに、ゲーム向きの部品を持った独自文法でも書けます。バックエンドは hlcc と共通なので、速さは同じです。

```
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
| 速度のため | `lut fn`（コンパイル時に表にする関数）、`inline fn`（呼ぶ所に展開。とても小さい関数は自動）、`next(p)`（読んで進める）、`copy` `outs`（`ldir` `otir`）。1バイトずつのコピー、塗りつぶし、ポートへの読み書きのループは、自動で `ldir` `lddr` `otir` `inir` などにする（[docs/05-codegen.md](docs/05-codegen.md)） |
| 機種とのつなぎ | `extern fn`（BIOS の呼び出し）、`port[n]`、`&関数名`、`ei` `di` `halt`、`import` |

言語の詳しい説明は [docs/03-language.md](docs/03-language.md)、タスクは [docs/07-tasks.md](docs/07-tasks.md) にあります。

## 目指すもの

- **HL を軸にした速いコード**：遅い IX/IY ではなく、HL でメモリを歩きます。変数の置き場所と呼び出し規約は、実際にコードを出して測りながら関数ごとに決まります。
- **待ちや再開を順番どおりに書けること**：`wait` と `yield` を使うと、「敵を出す、少し待つ、次を出す」を、状態番号と分岐を手で管理せずに書けます。
- **インタプリタを綺麗に速く書けること**：音楽ドライバのバイトコードマシンやゲーム進行のスクリプトを、列挙型と網羅性チェック付きの `match` で書けます。`match` は、順に比べる・2分探索・表で飛ぶ、のうち安いものが選ばれます。
- **出力が読めること**：生成されるアセンブリは、継続（引数を持つラベル）ごとのブロックで、各ブロックの先頭に値の置き場所がコメントで付きます。

ヒープも GC も再帰もありません。変数は固定の番地に置かれるので、スタックフレームは要らず、タスクが使うメモリの大きさもコンパイル時に分かります。

## 使い方

### リリースビルドを使う（OCaml は要らない）

GitHub の [Releases](https://github.com/hsk/HLang/releases) に、ビルド済みの `hlc`（hlang のコンパイラ）と `hlcc`（C のコンパイラ）を置いています。OCaml も dune も要りません。

| ファイル | 動く環境 |
| --- | --- |
| `hlang-vX.Y.Z-windows.zip` | Windows 10 / 11（64 ビット）。Windows に付いている DLL だけで動く |
| `hlang-vX.Y.Z-macos-arm64.tar.gz` | macOS（Apple シリコン） |
| `hlang-vX.Y.Z-linux-x86_64.tar.gz` | Linux（x86-64）。glibc 2.38 以上（Ubuntu 24.04 以降など） |

展開すると、`hlang` フォルダが 1 つできます。

```
hlang/
  hlc, hlcc        コンパイラ（Windows は hlc.exe, hlcc.exe）
  lib/             ライブラリ（msx.hl, sms.hl）。import "msx.hl" はここを探す
  examples/        例
  README.md, LICENSE
```

**中身の並びは変えないでください。** コンパイラは `import "msx.hl"`（C では `#include "msx.hl"`）のライブラリを、ソースのファイルの隣、その下の `lib/`、一つ上の `lib/` の順に探します。`examples/` の中のファイルは、一つ上の `lib/` が見つかるので、そのまま通ります。

#### Windows（PowerShell）

WSL も OCaml も要りません。PowerShell（Windows PowerShell 5.1 でも、PowerShell 7 でも）で次のようにします。

1. Releases から `hlang-vX.Y.Z-windows.zip` を落とします。

2. ダウンロードの印を外してから展開します。エクスプローラーで右クリックして「すべて展開」でもかまいません。展開先は、空白や日本語を含まない場所（例: `C:\hlang`）にすると、後でコマンドを打つのが楽です。

   ```powershell
   cd $HOME\Downloads
   Unblock-File .\hlang-v0.4.0-windows.zip
   Expand-Archive .\hlang-v0.4.0-windows.zip -DestinationPath C:\
   cd C:\hlang
   ```

   これで `C:\hlang\hlc.exe`、`C:\hlang\hlcc.exe`、`C:\hlang\lib\` ができます。

3. 日本語のメッセージが文字化けしないように、文字コードを UTF-8 にします（コンパイラのメッセージは UTF-8 です）。PowerShell を開くたびに 1 回要ります。

   ```powershell
   chcp 65001
   [Console]::OutputEncoding = [Text.Encoding]::UTF8
   ```

4. 動かしてみます。今のフォルダにある実行ファイルは、`.\` を付けて呼びます。

   ```powershell
   .\hlcc.exe examples\hello.c --run      # HELLO FROM C 55 と出る
   .\hlc.exe examples\hello.hl --run      # HELLO, HLANG! と出る
   .\hlc.exe examples\game.hl             # examples\game.rom ができる
   .\hlc.exe examples\sms_game.hl -sms    # examples\sms_game.sms ができる
   .\examples\build.ps1                   # examples の ROM を全部作る
   ```

   `build.ps1` が「スクリプトの実行が無効」と止められたら、その PowerShell の中だけ許します: `Set-ExecutionPolicy -Scope Process Bypass`。

5. どこからでも `hlc` と打てるようにするには、PATH に足します。今の PowerShell だけなら 1 行目、ずっと使うなら 2 行目です（2 行目の後は PowerShell を開き直します）。

   ```powershell
   $env:Path += ";C:\hlang"
   [Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", "User") + ";C:\hlang", "User")
   ```

コマンドプロンプト（cmd.exe）でも、同じように `hlc.exe examples\game.hl` で動きます（`.\` は要りません）。

ソースからのビルドや、エミュレータを使った確かめ方は [docs/windows.md](docs/windows.md)、WSL で使うときは [docs/wsl.md](docs/wsl.md) にあります。

#### macOS・Linux

```sh
tar xzf hlang-v0.4.0-macos-arm64.tar.gz
cd hlang
./hlcc examples/hello.c --run     # C の例を、内蔵のエミュレータで走らせる（HELLO FROM C 55 と出る）
./hlc examples/hello.hl --run     # hlang の例（HELLO, HLANG! と出る）
./hlc examples/game.hl            # examples/game.rom（MSX の ROM カートリッジ）ができる
```

macOS で、ブラウザで落とした物を実行すると「開発元を検証できない」と止められることがあります。そのときは、展開したフォルダの印を外します。

```sh
xattr -dr com.apple.quarantine hlang
```

#### 自分のフォルダで書く

`hlang` フォルダの外（例: `C:\work\mygame\main.hl`、`~/work/mygame/main.hl`）でソースを書くときは、ライブラリの場所を環境変数 `HLANG_LIB` で教えます。教えないと `import するファイルがない: msx.hl` になります。

```sh
# macOS・Linux（~/.zshrc や ~/.bashrc に書いておくと毎回要らない）
export HLANG_LIB=$HOME/hlang/lib
~/hlang/hlc main.hl
```

```powershell
# Windows（今の PowerShell だけ）
$env:HLANG_LIB = "C:\hlang\lib"
C:\hlang\hlc.exe main.hl
# ずっと使うなら（PowerShell を開き直してから効く）
[Environment]::SetEnvironmentVariable("HLANG_LIB", "C:\hlang\lib", "User")
```

`hlcc` は `-I` でも場所を足せます（`hlcc -I$HOME/hlang/lib main.c`）。`lib/` を自分のフォルダに写しても動きます。

#### コマンド

`hlc`（hlang）:

| コマンド | すること |
| --- | --- |
| `hlc file.hl` | `file.rom`（MSX の 16KB ROM カートリッジ。入らなければ 32KB）を、ソースと同じフォルダに作る |
| `hlc file.hl -sms` | `file.sms`（セガ・マスターシステムの 32KB ROM）を作る |
| `hlc file.hl --run` | 内蔵のエミュレータで走らせ、MSX の文字出力（CHPUT）を表示する |
| `hlc file.hl --asm` | 出すアセンブリを表示する（`--list` は番地と機械語つき、`--syms` はラベルの番地） |
| `hlc file.hl --report` | 関数ごとの大きさ、呼び出し規約、時間の上限、RAM の使い道を表示する |
| `-O0` | 最適化しない（変数をすべてメモリに置く）。コンパイルは一瞬 |
| `-r800` | 掛け算に R800 の命令（`mulub`、`muluw`）を使う。Z80 では動かない |

`hlcc`（C）:

| コマンド | すること |
| --- | --- |
| `hlcc main.c a.c b.c` | ファイルをまとめて 1 つのプログラムにし、最初のファイルの名前で ROM を作る（main.c なら main.rom） |
| `hlcc ... -Ipath` | `#include "..."` を探す場所を足す |
| `-sms`、`-O0`、`-r800`、`--run`、`--asm`、`--list`、`--syms` | `hlc` と同じ |
| `hlcc ... -megarom -bank a.bin -bank b.bin` | メガロム（ASCII8）を作る。プログラムは 0x4000〜0x9fff（24KB）、`-bank` のファイルは 8KB ごとのバンク（3 番から）として後ろに足す。openMSX では `-romtype ASCII8` を付ける |

ROM を作ると、`examples/game.rom: 1348 バイト` のように、作ったファイルと大きさが出ます。エラーは `ファイル:行: 内容` の形で出て、終了コードが 0 以外になります。

#### できた ROM を動かす

ROM は実機やエミュレータでそのまま動きます。MSX の ROM は BIOS の要らない C-BIOS で動きます。

- **openMSX**（<https://openmsx.org>。C-BIOS が付いてくる）: `openmsx -machine C-BIOS_MSX1 -cart examples/game.rom`。Windows はインストール先を付けて `& "C:\Program Files\openMSX\openmsx.exe" -machine C-BIOS_MSX1 -cart examples\game.rom`。
- **MAME**（SMS）: `mame sg1000m3 -cart examples/sms_game.sms -window`
- **WebMSX**（ブラウザ）: <https://webmsx.org> に ROM のファイルを落とすと動きます。

`examples/` で `make game`（openMSX）や `make sms`（MAME）も使えます。macOS・Linux で make がある場合です。リリースの中では、隣の `hlc` を自動で使います。

### ソースからビルドする

OCaml 5 以上と dune が要ります。

```
dune build

dune exec bin/hlcc.exe -- examples/hello.c --run   # C の例を走らせる

dune exec bin/hlc.exe -- examples/hello.hl         # hello.rom（MSX の 16KB ROM）
dune exec bin/hlc.exe -- examples/sms_game.hl -sms # sms_game.sms（SMS の 32KB ROM）
dune exec bin/hlc.exe -- examples/sum.hl --asm     # アセンブリを見る
dune exec bin/hlc.exe -- examples/hello.hl --run   # 内蔵のエミュレータで走らせる
dune exec bin/hlc.exe -- examples/game.hl --report # 関数ごとの大きさ、呼び出し規約、時間の上限、RAM

dune test                                          # 全テスト
```

```
dune exec bin/hlcc.exe -- examples/hello.c --run   # C の例を走らせる
```

Windows では、WSL2 の中（[docs/wsl.md](docs/wsl.md)）でも、ネイティブの PowerShell（[docs/windows.md](docs/windows.md)）でもビルドできます。

例は [examples/](examples/) にあります。`cd examples && make game` で openMSX が、`make sms` で MAME が起動します。

| ファイル | 内容 |
| --- | --- |
| [hello.c](examples/hello.c) | C で書いた例（hlcc） |
| [damaxian_c/](examples/damaxian_c/) | DAMAXIAN（MSX）の C 版。`make run` は SDCC、`make run2` は hlcc |
| [damaxian_hl/](examples/damaxian_hl/) | DAMAXIAN（MSX）の hlang 版。`make run` |
| [billboard_c/](examples/billboard_c/) | V9968 + geo3d のビルボード（C、hlcc）。VRAM 4MB のバンクにアニメーションのコマを置く |
| [race_c/](examples/race_c/) | V9968 + geo3d v2.0 で、空に浮いたコースを走るレース（C、hlcc）。コースは geo3d に置いたまま |
| [hello.hl](examples/hello.hl) | 文字を出す |
| [script.hl](examples/script.hl) `vm.hl` | バイトコードマシン |
| [music.hl](examples/music.hl) `tracks.hl` | 曲のバイトコードをタスクが読んで、PSG を鳴らす |
| [game.hl](examples/game.hl) | 小さなシューティング（MSX）。敵と弾のプール、当たり判定、スプライト、割り込みの曲 |
| [sms_game.hl](examples/sms_game.hl) | 同じゲームの SMS 版 |

ライブラリは [lib/msx.hl](lib/msx.hl) と [lib/sms.hl](lib/sms.hl) です。使わなかった関数は ROM に入りません。

## 検証

- エミュレータは zexdoc・zexall の全67項目が通ります。アセンブラは全697命令で `z80asm` と一致します。
- 同じプログラムを、参照インタプリタと、コンパイルした機械語（エミュレータ上）の両方で実行して、出力を突き合わせます。プログラムは乱数でも作ります。
- ゲームは openMSX と MAME でも動かして確かめています。

## 文書

- [docs/11-c-frontend.md](docs/11-c-frontend.md)：C フロントエンド（hlcc）の仕様
- [docs/making.md](docs/making.md)：コンパイラの作り方（章立ての入口）
- [docs/wsl.md](docs/wsl.md)：Windows（WSL）で使う
- [docs/windows.md](docs/windows.md)：Windows のネイティブ（PowerShell）で使う
- [docs/09-zma.md](docs/09-zma.md)：ZMA（HRA! さんのアセンブラ）の書き方を読む・書く
- [plan.md](plan.md)：計画と進み具合、残っていること
- [concept.md](concept.md)：構想と、先行する実装の調査記録

## ライセンス

MIT ライセンスです（[LICENSE](LICENSE)）。コンパイラ、`lib/` のライブラリ、`examples/`、文書のすべてが対象です。hlang で作った ROM には、`lib/` から取り込んだコードが入りますが、MIT なので、ROM を配るときに特別な手続きは要りません（著作権の表示は LICENSE のとおり）。
