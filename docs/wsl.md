# Windows（WSL）で使う

hlang のコンパイラは OCaml だけで書かれていて、OS の機能を呼んでいません。シェルスクリプトも POSIX の `sh` です。そのため Windows では WSL2（Ubuntu）の中で、Linux と同じ手順で動かします。Windows のネイティブ（PowerShell）で使う場合は [windows.md](windows.md) です。

> この手順は macOS で作った構成から書いたもので、WSL 上ではまだ通して確かめていません。確かめた結果は、末尾の「確認欄」に書き込んでください。

## 1. WSL を入れる

管理者の PowerShell で、1 回だけ。

```powershell
wsl --install -d Ubuntu
```

再起動して Ubuntu を開き、ユーザー名とパスワードを決めます。以降の操作は、すべて Ubuntu の中です。

## 2. 道具を入れる

```sh
sudo apt update
sudo apt install -y git build-essential opam
opam init -y --bare
opam switch create hlang 5.2.0        # OCaml 5 以上が要る（タスクの参照インタプリタが Effect を使う）
eval $(opam env)
opam install -y dune
```

`ocaml -version` が 5.x、`dune --version` が 3.0 以上であることを確かめます。`opam init` が bubblewrap の警告を出すときは、`opam init -y --bare --disable-sandboxing` で入れ直します（WSL1 や、一部の WSL2 で必要）。

## 3. 取ってきて動かす

**リポジトリは WSL の中（`~/` の下）に置きます。** `/mnt/c/...`（Windows 側のディスク）に置くと、ビルドが数倍遅くなり、改行コードの食い違いでシェルスクリプトが動かなくなることがあります。

```sh
cd ~
git clone https://github.com/hsk/HLang.git
cd HLang
dune build
dune test                                    # 数分かかる。乱数の件数を減らすなら HL_RANDOM=20 dune test
dune exec bin/hlc.exe -- examples/hello.hl --run
dune exec bin/hlc.exe -- examples/sms_game.hl -sms    # examples/sms_game.sms ができる
```

ソースは Windows のエディタで編集できます。VS Code なら、拡張機能「WSL」で `code .` と打てば、WSL の中のフォルダをそのまま開けます。Windows のエクスプローラーからは `\\wsl$\Ubuntu\home\<ユーザー名>\HLang` で見えます。

## 4. エミュレータ

画面を見たいとき、または画面を撮って確かめたいときに入れます。コンパイルとテストには要りません（エミュレータは自前のものが内蔵されています）。

```sh
sudo apt install -y openmsx mame
```

- **画面を開いて動かす**：Windows 11（と、更新した Windows 10）には WSLg が入っていて、Linux の GUI アプリがそのままウィンドウで開きます。`cd examples && make game`（MSX）、`make sms`（SMS）。音が出ない・遅いときは、ヘッドレスで確かめる方を使ってください。
- **ヘッドレスで確かめる**：`tools/msx-screen.sh examples/hello.rom`（openMSX で起動して画面の文字を読む）、`tools/sms-screen.sh examples/sms_game.sms`（MAME で画面を PNG に撮る）。どちらも画面を開きません。

MSX は、BIOS の要らない C-BIOS で動きます（openMSX に付いてきます）。SMS は MAME の `sg1000m3`（マーク III）で動かすので、BIOS を別に用意する必要はありません。

## 5. ベンチマーク（なくても困らない）

```sh
sudo apt install -y z80asm sdcc
bench/run.sh        # SDCC との比較
bench/vm.sh         # 手書きアセンブリとの比較
```

Ubuntu の `sdcc` は、README に書いた 4.6 とは版が違うかもしれません。数字が少し違うのは、そのためです。`dune test` の中の「`z80asm` と一致」の検査は、`z80asm` が入っていれば走り、なければ飛ばされます。

## よくあること

| 症状 | 原因と直し方 |
| --- | --- |
| `bad interpreter: /bin/sh^M` | 改行が CRLF になっている。Windows 側の Git で clone したのが原因。WSL の中で clone し直す（リポジトリには `.gitattributes` で LF を指定してある） |
| ビルドが遅い | `/mnt/c` に置いている。`~/` の下へ移す |
| `Effect` が見つからない | OCaml が 4.x。`opam switch create hlang 5.2.0` で 5 以上にする |
| MAME や openMSX のウィンドウが開かない | `wsl --update` で WSLg を更新して `wsl --shutdown`。それでも出なければ、ヘッドレスの 2 つのスクリプトを使う |
| `tools/*.sh` が `mktemp /tmp/...` で失敗する | `/tmp` に書けない設定になっている。まれ。`TMPDIR` を使うよう直すので、知らせてください |

## 確認欄

通しで確かめたら、環境と結果を追記してください。

| 日付 | 環境 | `dune test` | MSX の画面 | SMS の画面 | 備考 |
| --- | --- | --- | --- | --- | --- |
| | | | | | |
