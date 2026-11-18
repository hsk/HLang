# DAMAXIAN MSX SDCC & HLCC

（hlang のリポジトリに入れた写しです。元は [hsk/damaxian_msx](https://github.com/hsk/damaxian_msx)。hlang 版は [../damaxian_hl](../damaxian_hl) にあります。元はディスク（BASIC から BLOAD）で動かしますが、ここでは 16KB の ROM カートリッジにして、C-BIOS で動かします。変えたのは起動コード `sources/crt0.s` と makefile だけです。）

DAMAXIAN MSX SDCC & HLCC はMSX上で動作するシューティングゲームです。
オリジナルはcobineeさんのオールアセンブラで作られているものでそれをC言語化しました。
固定小数点数を用いたレトロなゲーム機でも動作するプログラムの経験を積むための学習用コードとして再編集しました。

元のリポジトリでは、履歴が 15 の段階（自機を動かす、敵を出す、……、音をつける）に分けてあり、順を追って写経できます。ここには最後の段階のソースだけを入れています。段階ごとの差分は元のリポジトリで見てください。

## ビルドと実行

SDCC（4.6 で確認）と openMSX が要ります。

```sh
make        # SDCC で bin/DAMAXIAN.rom（16KB の ROM カートリッジ）を作る
make run    # ビルドして openMSX（C-BIOS_MSX1）で動かす。タイトルで SPACE を押すと始まる
make run2   # hlang の C フロントエンド（../../bin/hlcc.ml）で bin/DAMAXIAN2.rom を作り、openMSX（C-BIOS_MSX1）で動かす
```

`make rom2` は ROM を作るだけです。hlcc は、ひとつ上の hlang のリポジトリで `dune build` して使います。

## 速さと大きさ

同じ C のソースを、SDCC と hlcc でビルドして比べました。hlang 版（[../damaxian_hl](../damaxian_hl)）とアセンブラ版（[../damaxian_asm](../damaxian_asm)）も並べます。

物差しは、タイマ割り込みの処理 1 回（= 1 ゲームフレーム）の所要時間です。左右に動いて連射する入力で、乱数の種を変えて 4 回（各 3000 フレーム）、openMSX（C-BIOS_MSX1）で走らせました。1/60 秒は 59,659T で、これを超えるとゲームが 30fps に落ちます。測り方は [../damaxian_hl/tools/bin-time.sh](../damaxian_hl/tools/bin-time.sh)（`tools/bin-time.sh sdcc`、`tools/bin-time.sh hlcc`）にあります。2026-11-19 に測った数字です。

| | 平均 | 95% | 最大 | 1/60 秒超え |
| --- | --- | --- | --- | --- |
| C 版、SDCC 4.6.0（`--opt-code-speed`） | 68,570T | 91,715T | 116,305T | 73.8% |
| C 版、hlcc | **43,612T** | 56,420T | 74,726T | 1.6% |
| hlang 版 | 42,871T | 55,528T | 86,749T | 1.5% |
| アセンブラ版 | 46,541T | 57,618T | 89,654T | 2.8% |

| | C 版、SDCC | C 版、hlcc | hlang 版 | アセンブラ版 |
| --- | --- | --- | --- | --- |
| コード + 表 | 14,818 バイト | 14,608 バイト | 14,080 バイト | 13,784 バイト |
| RAM | 981 バイト（スタック 256 を含む） | 848 バイト | 768 バイト | 1,000 バイト（スタック 256 を含む） |

- **同じソースでも、hlcc の処理時間は SDCC の 64%（36% 短い）。** SDCC 版は、敵の撃ち返しが増える場面で 1/60 秒に収まらず、4 フレームに 3 フレームは遅れる。hlcc 版はほとんど収まり、手書きのアセンブラ版より 6% ほど短い。
- hlcc 版と hlang 版の差（約 740T、2%）の多くは、hlang 版のソースの手直し（弾のスプライト位置を持ち歩く、VDP のレジスタを毎フレーム書き直さない）によるもので、コンパイラの差ではない（[../damaxian_hl/README.md](../damaxian_hl/README.md) の「元との違い」）。
- hlcc 版の RAM のうち 96 バイトは、関数の中の `static const` の表（`Game.c` の `backSpeedTable` など）。hlcc は今これを RAM に置く。
- SDCC 版のスタック 256 バイトは、`crt0.s` が確保する分。hlcc 版と hlang 版のスタックは、ここに入っていない。

## アセンブラ版との違い

C 版のソースには、アセンブラ版と動きが違う箇所があります（[../damaxian_hl/README.md](../damaxian_hl/README.md) の「アセンブラ版との一致」）。hlang 版はアセンブラ版に合わせてありますが、ここの C のソースは元のままです。上の速さは、元のソースのままで測りました。

