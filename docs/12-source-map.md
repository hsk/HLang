# ソースファイルと機能の対応表

どのファイルが何をしていて、どれくらいの大きさか。行数は `wc -l` の値（コメントと空行を含む。2026-11 時点）。
各段の中身は [making.md](making.md) の章で説明しています。

合計は、コンパイラ本体（src）が約 9,600 行、エミュレータが約 500 行、コマンドが約 200 行、テストが約 1,800 行です。

## エミュレータ（emu/）

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [emu/z80.ml](../emu/z80.ml) | 496 | Z80 エミュレータ。T ステートも数える。検証の土台 | [01](01-emulator.md) |

## 命令の表現（アセンブラ側）

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/asm.ml](../src/asm.ml) | 320 | Z80 アセンブラ。命令をデータで持ち、機械語とテキストにする | [02](02-assembler.md) |
| [src/dis.ml](../src/dis.ml) | 193 | 逆アセンブラ。アセンブラの逆関数（往復で両方を検査） | [02](02-assembler.md) |
| [src/cost.ml](../src/cost.ml) | 78 | 命令のコスト。組み立てた長さと、エミュレータで測った時間 | [02](02-assembler.md) |
| [src/zma.ml](../src/zma.ml) | 410 | ZMA 互換の書き方を読む（マクロ、REPEAT、IF、SCOPE） | [09](09-zma.md) |

## 言語の表側（字句 → 型検査）

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/lex.ml](../src/lex.ml) | 76 | 字句解析 | [03](03-language.md) |
| [src/ast.ml](../src/ast.ml) | 88 | 構文木の型定義 | [03](03-language.md) |
| [src/parse.ml](../src/parse.ml) | 278 | 構文解析（再帰下降） | [03](03-language.md) |
| [src/check.ml](../src/check.ml) | 781 | 型検査、定数畳み込み、変数名の一意化、match の網羅性チェック | [03](03-language.md), [06](06-interpreter.md) |
| [src/interp.ml](../src/interp.ml) | 246 | 参照インタプリタ。言語の意味の基準（コンパイル結果と比べる） | [03](03-language.md), [07](07-tasks.md) |

## 中間表現

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/ir.ml](../src/ir.ml) | 103 | 中間表現（関数と継続だけ）の型定義 | [04](04-cps.md) |
| [src/cps.ml](../src/cps.ml) | 318 | 構文木から継続だけの中間表現への変換 | [04](04-cps.md) |
| [src/live.ml](../src/live.ml) | 177 | 生存解析（各地点で後に使われる変数）。最適化とコード生成が使う | [04](04-cps.md) |

## 最適化

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/const_fold.ml](../src/const_fold.ml) | 32 | 定数畳み込み。式 1 つを見て、定数だけなら値にする（ラベルの番地 + 定数も） | [04](04-cps.md) |
| [src/cse.ml](../src/cse.ml) | 92 | 共通部分式の除去。同じ計算と同じ番地の読みを 1 回にする。畳み込みも呼ぶ | [04](04-cps.md) |
| [src/inline.ml](../src/inline.ml) | 108 | 小さな関数のインライン展開 | [05](05-codegen.md) |
| [src/narrow.ml](../src/narrow.ml) | 148 | 16bit の計算を 8bit に狭める | [05](05-codegen.md) |
| [src/ptr.ml](../src/ptr.ml) | 192 | 添字をポインタに直す | [06](06-interpreter.md) |
| [src/iv.ml](../src/iv.ml) | 154 | 構造体配列を歩くポインタ（帰納変数の強度低減） | [06](06-interpreter.md) |
| [src/blk.ml](../src/blk.ml) | 168 | コピー・塗りつぶしのループを ldir / lddr などにする | [05](05-codegen.md) |
| [src/lut.ml](../src/lut.ml) | 88 | `lut fn` をコンパイル時に表にする（構文木の段） | [03](03-language.md) |

## コード生成

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/gen.ml](../src/gen.ml) | 1299 | 中間表現から Z80 の命令列を作る。最大のファイル | [05](05-codegen.md) |
| [src/alloc.ml](../src/alloc.ml) | 807 | 変数の置き場所の探索。呼び出し規約を関数ごとに決める | [05](05-codegen.md) |
| [src/report.ml](../src/report.ml) | 90 | 時間の上限（入口から ret までの最長 T ステート）のレポート | [05](05-codegen.md) |
| [src/run.ml](../src/run.ml) | 40 | 機械語をエミュレータで走らせる。BIOS 代わりの「わな」 | [08](08-msx.md) |
| [src/hlang.ml](../src/hlang.ml) | 203 | コンパイラの入口。ソースから ROM までをつなぐ | — |

## C フロントエンド

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [src/cpp.ml](../src/cpp.ml) | 390 | C の字句解析とプリプロセッサ | [11](11-c-frontend.md) |
| [src/cparse.ml](../src/cparse.ml) | 486 | C の構文解析（typedef を覚えながら読む） | [11](11-c-frontend.md) |
| [src/clower.ml](../src/clower.ml) | 1290 | C の構文木を、C の意味のまま hlang の構文木に直す | [11](11-c-frontend.md) |
| [src/casm.ml](../src/casm.ml) | 142 | SDCC のインラインアセンブラを組み立ての行にする | [11](11-c-frontend.md) |

## コマンド（bin/）

| ファイル | 行 | 機能 |
| --- | ---: | --- |
| [bin/hlc.ml](../bin/hlc.ml) | 131 | hlang コンパイラ。ROM 作成、`--asm` `--ir` など |
| [bin/hlcc.ml](../bin/hlcc.ml) | 75 | C 版のコマンド。同じバックエンドを使う |

## ライブラリ（lib/）

| ファイル | 行 | 機能 | 章 |
| --- | ---: | --- | --- |
| [lib/msx.hl](../lib/msx.hl) | 100 | MSX の BIOS と I/O ポート | [08](08-msx.md) |
| [lib/sms.hl](../lib/sms.hl) | 121 | セガ・マスターシステムの VDP、PSG、パッド | [08](08-msx.md) |

## テスト（test/）

| ファイル | 行 | 内容 |
| --- | ---: | --- |
| [test/t_z80.ml](../test/t_z80.ml) | 112 | エミュレータ。小さなプログラムの結果と T ステート |
| [test/zex.ml](../test/zex.ml) | 25 | zexdoc / zexall（命令網羅テスト）を走らせる |
| [test/tstates.ml](../test/tstates.ml) | 19 | 全公式命令の T ステート表を出す（他の資料との突き合わせ用） |
| [test/t_asm.ml](../test/t_asm.ml) | 89 | アセンブラと逆アセンブラの往復、外部 z80asm との一致 |
| [test/t_zma.ml](../test/t_zma.ml) | 125 | ZMA の読み込みと逆アセンブルの往復 |
| [test/t_lang.ml](../test/t_lang.ml) | 1087 | 言語。インタプリタとエミュレータの出力一致、乱数プログラム |
| [test/t_c.ml](../test/t_c.ml) | 55 | C フロントエンド。`test/c/*.c` を clang の出力と比べる |
| [test/t_sms.ml](../test/t_sms.ml) | 97 | SMS の VDP・PSG・パッドを模して走らせる |
| [test/runbin.ml](../test/runbin.ml) | 15 | 生のバイナリを HALT まで走らせて時間を出す |
| [test/sw.ml](../test/sw.ml) | 45 | match の出し方（順・2 分探索・表）の測定 |

## 読む順番の目安

1. 全体像: [hlang.ml](../src/hlang.ml)（入口、200 行）
2. 表側: lex → parse → check（約 1,100 行）
3. 中間表現と最適化の入口: ir → cps → live → cse（約 700 行）
4. 裏側: alloc → gen（約 2,100 行。ここが本体）
5. C を読むなら: cpp → cparse → clower
