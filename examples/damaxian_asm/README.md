# DAMAXIAN

（hlang のリポジトリに入れた写しです。元は [CoBinee/damaxian-msx](https://github.com/CoBinee/damaxian-msx)。全部アセンブラで書かれた原作で、hlang 版 [../damaxian_hl](../damaxian_hl) の動きと速さの基準にしています。ここでは、ディスク（BASIC から BLOAD）ではなく 16KB の ROM カートリッジにして、C-BIOS で動かします。変えたのは起動コード `sources/crt0.s` と makefile だけです。ディスクイメージ diskimage/ は入れていません。）

[![DAMAXIAN](http://img.youtube.com/vi/VfeDRP2QGs0/0.jpg)](https://www.youtube.com/watch?v=VfeDRP2QGs0)

## 説明
倍率を上げ続けてください。<br>
こちらから遊べます。⇒  [WebMSX](http://webmsx.org/?MACHINE=MSX1J&DISK=https://github.com/CoBinee/damaxian-msx/raw/main/diskimage/damaxian.dsk)

## 操作
- `←`, `→`<br>移動
- `SPACE`<br>発射
- `ESX`<br>一時停止
- ジョイスティックにも対応

## 動作環境
- 32KB 以上の RAM を搭載した MSX

## 開発
- [SDCC](https://sdcc.sourceforge.net)

## ビルドと実行

SDCC（4.6 で確認。付属の `sdasz80` と `sdld` を使う）と openMSX が要ります。

```sh
make        # bin/DAMAXIAN.rom（16KB の ROM カートリッジ）を作る
make run    # ビルドして openMSX（C-BIOS_MSX1）で動かす。タイトルで SPACE を押すと始まる
```

速さの測り方は [../damaxian_hl/tools/bin-time.sh](../damaxian_hl/tools/bin-time.sh)（`tools/bin-time.sh asm`）にあります。
