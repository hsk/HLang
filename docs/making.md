# hlang の作り方（making）

hlang は Z80 向けの小さなコンパイル言語です。この文書は、コンパイラをどう作ったかを順に説明します。各章が、ソースのファイル 1〜2 個に対応します。

| 章 | 内容 | ソース |
| --- | --- | --- |
| [1. エミュレータ](01-emulator.md) | Z80 を動かす。検証の土台 | [emu/z80.ml](../emu/z80.ml) |
| [2. アセンブラと逆アセンブラ](02-assembler.md) | 命令をデータで持ち、機械語とテキストにする | [src/asm.ml](../src/asm.ml), [src/dis.ml](../src/dis.ml), [src/cost.ml](../src/cost.ml) |
| [3. 言語の表側](03-language.md) | 字句解析、構文解析、型検査、参照インタプリタ | [src/lex.ml](../src/lex.ml), [src/parse.ml](../src/parse.ml), [src/check.ml](../src/check.ml), [src/interp.ml](../src/interp.ml) |
| [4. 継続だけの中間表現](04-cps.md) | 分岐もループも「継続へ飛ぶ」にする | [src/ir.ml](../src/ir.ml), [src/cps.ml](../src/cps.ml), [src/live.ml](../src/live.ml), [src/opt.ml](../src/opt.ml) |
| [5. コード生成と置き場所の探索](05-codegen.md) | HL を活かす。呼び出し規約を関数ごとに決める | [src/gen.ml](../src/gen.ml), [src/alloc.ml](../src/alloc.ml) |
| [6. インタプリタを書くための部品](06-interpreter.md) | 列挙型、網羅性チェック付きの match（出し方は測って選ぶ）、next、表と配列、添字をポインタに直す最適化、手書きとの比較 | [src/check.ml](../src/check.ml), [src/cps.ml](../src/cps.ml), [src/gen.ml](../src/gen.ml), [src/ptr.ml](../src/ptr.ml) |

| [7. タスク](07-tasks.md) | yield、wait、spawn、resume。途中で止まって続きから動く | [src/gen.ml](../src/gen.ml), [src/interp.ml](../src/interp.ml) |
| [8. MSX と SMS で動かす](08-msx.md) | 画面と音。import、関数の番地、割り込み、ライブラリ。セガ・マスターシステム版 | [lib/msx.hl](../lib/msx.hl), [lib/sms.hl](../lib/sms.hl), [examples/game.hl](../examples/game.hl), [examples/sms_game.hl](../examples/sms_game.hl) |

## 全体の流れ

```text
ソース ──字句・構文──▶ 構文木 ──型検査──▶ 型付き構文木 ──CPS 変換──▶ 中間表現
                                         │                          │
                                  参照インタプリタ            置き場所の探索 + コード生成
                                         │                          │
                                         │                     命令のデータ ──▶ アセンブリのテキスト
                                         │                          │
                                         │                       機械語 ──▶ エミュレータ
                                         └────── 出力を比べる ──────┘
```

右下の「出力を比べる」が、このコンパイラの検証の中心です。同じプログラムを参照インタプリタと機械語の両方で実行し、結果が一致することを確かめます。プログラムは乱数でも作ります。

## 動かし方

```sh
dune build
dune test                                  # 全部のテスト
dune exec bin/hlc.exe -- examples/sum.hl --asm    # アセンブリを見る
dune exec bin/hlc.exe -- examples/sum.hl --ir     # 中間表現を見る
dune exec bin/hlc.exe -- examples/hello.hl --run  # エミュレータで実行
dune exec bin/hlc.exe -- examples/game.hl --report  # 大きさ、呼び出し規約、時間の上限
dune exec bin/hlc.exe -- examples/hello.hl        # hello.rom を作る（MSX の 16KB ROM）
tools/msx-screen.sh examples/hello.rom            # openMSX で起動して画面の文字を読む
dune exec bin/hlc.exe -- examples/sms_game.hl -sms  # sms_game.sms を作る（SMS の 32KB ROM）
tools/sms-screen.sh examples/sms_game.sms         # MAME で起動して画面を撮る
```

`-O0` を付けると最適化なし（変数をすべてメモリに置く）のコードが出ます。比べると、探索が何をしたかが分かります。
