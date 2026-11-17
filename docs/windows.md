# Windows のネイティブで使う（WSL なし）

PowerShell だけで、コンパイラのビルドから ROM 作り、テストまでできます。WSL を使う場合は [wsl.md](wsl.md) です。

> macOS で作った構成から書いた手順です。ビルドとテストと ROM 作りは、GitHub Actions（[.github/workflows/ci.yml](../.github/workflows/ci.yml)）の `windows-latest` で通ることを確かめました。この文書の PowerShell のスクリプト（`build.ps1`、`sms-screen.ps1`）と、エミュレータを使う部分は、まだ実行していません。手元で確かめた結果は、末尾の「確認欄」に書き込んでください。

## なぜ動くか

コンパイラ（`hlc`）とテストは、OCaml の標準ライブラリだけで書かれています。OS を呼ぶのは、ファイルの読み書きだけです。ROM は `open_out_bin`（バイナリ）で書くので、改行の変換で壊れることはありません。外部コマンドを呼ぶのは、`z80asm` との突き合わせ（Windows では飛ばします）だけです。

## 1. 道具を入れる

OCaml 5 以上と dune が要ります。2 つの入れ方があります。

**A. opam（OCaml 公式。Windows に対応）**

```powershell
winget install OCaml.opam
opam init -y
opam switch create hlang ocaml-base-compiler.5.2.0
opam install -y dune
```

`opam init` は、必要な C の道具（MSYS2 など）を自分で入れます。入れたあとは、`opam env` を現在のシェルに反映します。

```powershell
(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }
```

**B. 確かめ済みの構成を真似る**

CI は `ocaml/setup-ocaml` で OCaml 5.2 を入れて、`opam install dune` しています。手順に迷ったら、[ci.yml](../.github/workflows/ci.yml) が正解です。

`ocaml -version` が 5.x、`dune --version` が 3.0 以上であることを確かめます。

## 1.5 自分でビルドしない（GitHub Actions の成果物を使う）

GitHub の「Actions」タブで、最新の ci の実行を開き、下の「Artifacts」から `hlang-windows-latest` を取ります。`hlc.exe`、`lib/`、`examples/` が入っています。展開して、そのフォルダで `.\hlc.exe examples\hello.hl --run` を実行します。`hlc.exe` は `lib/` を隣から探すので、フォルダの中身は動かさないでください。

## 2. ビルドと実行

```powershell
git clone https://github.com/hsk/HLang.git
cd HLang
dune build
dune test                                   # 数分かかる。$env:HL_RANDOM = "20"; dune test で乱数の件数を減らせる
dune exec bin/hlc.exe -- examples/hello.hl --run
.\examples\build.ps1                        # examples の ROM を全部作る（MSX は .rom、SMS は .sms）
```

日本語のメッセージが文字化けするときは、先に `chcp 65001` を実行します。

**改行**：Git for Windows の初期設定（`core.autocrlf=true`）で clone しても、リポジトリの `.gitattributes` が LF を指定するので、`.hl` のソースもテストも同じ結果になります。

## 3. エミュレータ

画面を見たいときだけ入れます。コンパイルとテストには要りません。

- **openMSX**：<https://openmsx.org> のインストーラ。C-BIOS が付いてくるので、`openmsx -machine C-BIOS_MSX1 -cart examples\game.rom` で動きます。
- **MAME**：<https://www.mamedev.org> から取って、PATH に通します。SMS は `mame sg1000m3 -cart examples\sms_game.sms -window` で動きます（BIOS は要りません）。

画面を開かずに確かめるには：

```powershell
.\tools\sms-screen.ps1 examples\sms_game.sms     # MAME で画面を PNG に撮る
```

MSX の画面の文字を読む `tools/msx-screen.sh` は、まだ PowerShell 版がありません。WSL で使うか、openMSX を開いて目で見てください。

## できないこと

- `bench/run.sh` と `bench/vm.sh`（SDCC と `z80asm` が要るシェルスクリプト）。比較は WSL か Linux / macOS で。
- `dune test` の中の `z80asm` との突き合わせ。Windows では飛ばされます（697 命令の往復の検査は走ります）。

## 確認欄

| 日付 | 環境 | `dune build` | `dune test` | ROM | 備考 |
| --- | --- | --- | --- | --- | --- |
| 2026-11-17 | GitHub Actions `windows-latest`、OCaml 5.2（`setup-ocaml`） | 成功 | 成功（`HL_RANDOM=30`） | `game.rom` と `sms_game.sms` を作れた | 手元の Windows では未確認。CI の実行: 44051869821 |
