# DAMAXIAN MSX in hlang

`../damaxian_c`（SDCC で書かれた C 版）を、hlang に**ソースファイル単位で 1 対 1 に**移したものです。動きは、元の手書きアセンブラ版に合わせてあります（下の「アセンブラ版との一致」）。
ファイルの分け方、関数名、状態の遷移、固定小数点数、スプライトの並べ替え、MML の音楽ドライバまで、元と同じ作りにしてあります。

| 元（damaxian_c/sources） | ここ | 内容 |
| --- | --- | --- |
| main.c | [main.hl](main.hl) | タイマ割り込み（H.TIMI）で 1 フレームずつ進める。main は STOP キーを待つだけ |
| System.c / System.h | [System.hl](System.hl) | キー入力、ビデオレジスタ、VRAM 転送、スプライト転送 |
| vdp.h | [vdp.hl](vdp.hl) | VDP のレジスタ番号 |
| Sound.c / Sound.h | [Sound.hl](Sound.hl) | 4 チャンネルの MML インタプリタ、PSG への書き込み、MML データ |
| Math.c / Math.h | [Math.hl](Math.hl) | 乱数、sin / cos の表、atan2 |
| App.c / App.h | [App.hl](App.hl) | モードの切り替え、スコアなどの共有変数 |
| Back.c / Back.h | [Back.hl](Back.hl) | 背景、スコア・倍率・タイマの表示、メッセージ |
| Title.c / Title.h | [Title.hl](Title.hl) | タイトル画面 |
| Game.c / Game.h | [Game.hl](Game.hl) | ゲーム画面の状態遷移、当たり判定、5 方向の撃ち返し |
| Ship.c / Ship.h | [Ship.hl](Ship.hl) | 自機 |
| Shot.c / Shot.h | [Shot.hl](Shot.hl) | ショット |
| Enemy.c / Enemy.h | [Enemy.hl](Enemy.hl) | 敵（イン、待機、ターン、アプローチ、爆発） |
| Bullet.c / Bullet.h | [Bullet.hl](Bullet.hl) | 敵の弾 |
| pattern.c | [pattern.hl](pattern.hl) | フォントとスプライトの絵（生成） |
| bg.c | [bg.hl](bg.hl) | 背景、ロゴ、メッセージ、敵の位置（生成） |
| bios.h, bdos.h, bios.inc, crt0.s | （`lib/msx.hl`、hlang の起動コード） | BIOS 呼び出しと起動 |

ヘッダ（.h）の定数や構造体は、対応する .hl の先頭にあります。`import` の関係は元の `makefile.depend` と同じです。

## 元との違い

- **初期化は最初の割り込みで行う。** 元は main が `SystemInitialize` などを呼んでから割り込みを仕掛けます。hlang は「割り込みから呼ぶ関数」と「main から呼ぶ関数」を分ける決まりなので、`APP_MODE_INIT` のとき、最初の割り込みの `AppUpdate` が呼びます（`flag` は割り込み処理中の印を残して初期化）。
- **asm で書かれていた部分は hlang で書き直した。** `SystemUpdateInput`、`SystemTransfer*`、`SystemUpdateSound`、`SystemGetAtan2` など。分岐の癖（チャンネル A の音の有無を周波数の下位と上位の比較で決める、チャンネル D が鳴っている間はチャンネル C を出さない、など）も写しています。
- **`sprite` は `sprites`。** `lib/msx.hl` に同じ名前の関数があるためです。
- **速くするために、動きを変えずに次の 4 か所を変えた**（01-straight → 02-tuned で約 8%。下の「速さ」）。
  - `request` の印を、転送したら下ろす（元は下ろさず、同じレジスタ値を毎フレーム VDP に書き直す）。
  - `SystemTransferVram` は、転送の欄を番地で歩き、空きにするのは `src` だけ。
  - 弾と敵のスプライトの位置は、ループの中で 4 ずつ進めて持ち歩く（元は 1 つずつ計算し直す）。`Bullet` の `sprite_offset` は使わなくなったが、元との対応のため残してある。
  - `BackMakeDigits` は `next(p)` で読み進める。サウンドは、待ち時間のカウントダウンを呼び出し側で済ませる。
- **割り込みの最後の `jp h_timiRoutine`（保存した元のフックへ飛ぶ）はしない。** 保存だけして、終了時に戻します。
- **VDP のポートは 0x98 / 0x99 の固定値。** 元は 0x0006 / 0x0007 から読みます。
- 文字列の MML は、1 バイトずつの `data` にしてあります（MML の文字列は各ブロックのコメントに残してあります）。

## 作り方、遊び方

```sh
make          # main.rom（中身は 14,080 バイト。16KB の ROM）
make run      # openMSX で起動（MACHINE=... で機種を変える）
make assets   # pattern.hl と bg.hl を元の C から作り直す
```

カーソルキーの左右で動き、スペースで撃ちます。Esc で一時停止、STOP で終了です。

## 速さと大きさ（SDCC 版、アセンブラ版との比較）

比べた相手は 2 つです。どちらもこのリポジトリにあり、`make` で `bin/DAMAXIAN.rom`（16KB の ROM カートリッジ）を作ります。

- **SDCC 版**: [../damaxian_c](../damaxian_c)（C）を SDCC 4.6.0 でビルド（`--opt-code-speed`。`System.c`、`Sound.c`、`Math.c` などはインラインアセンブラ入り）。
- **アセンブラ版**: [../damaxian_asm](../damaxian_asm)（オリジナルの [CoBinee/damaxian-msx](https://github.com/CoBinee/damaxian-msx) の写し。全部アセンブラで、サウンドも含む）。SDCC 付属の `sdasz80` と `sdld` でビルド。

物差しは、**タイマ割り込みの処理 1 回（= 1 ゲームフレーム）の所要時間**です。左右に動いて連射する入力で、乱数の種を変えて 4 回（各 3000 フレーム）走らせ、まとめて数えます。処理中に重なって来た割り込みの、BIOS の処理時間は引いてあります。1/60 秒は 59,659T で、これを超えると割り込みが重なって、ゲームが 30fps に落ちます。「CPU の待ち時間の割合」は、フレームが落ちると待ちが増えて負荷が軽く見えるので使いません。

```sh
tools/handler-time.sh                        # hlang 版
tools/handler-time.sh versions/01-straight   # hlang 版（素直に写した版）
tools/bin-time.sh asm                        # アセンブラ版
tools/bin-time.sh sdcc                       # SDCC 版
tools/bin-time.sh hlcc                       # 同じ C を hlcc でビルドした版
```

どれも openMSX（C-BIOS_MSX1）で動かします。2026-11-19 に測った数字です。

| | 平均 | 95% | 最大 | 1/60 秒超え |
| --- | --- | --- | --- | --- |
| hlang 版（調整後。[versions/02-tuned](versions/02-tuned/) と同じ） | **42,871T** | 55,528T | 86,749T | 1.5% |
| hlang 版（素直に写した版。[versions/01-straight](versions/01-straight/)） | 46,596T | 59,072T | 90,356T | 4.1% |
| アセンブラ版 | 46,541T | 57,618T | 89,654T | 2.8% |
| SDCC 版 | 68,570T | 91,715T | 116,305T | 73.8% |

同じ C のソースを hlcc（hlang の C フロントエンド）でビルドすると、平均 43,612T です（[../damaxian_c](../damaxian_c) の README）。

| | hlang 版 | SDCC 版 | アセンブラ版 |
| --- | --- | --- | --- |
| コード + 表 | 14,080 バイト | 14,818 バイト | 13,784 バイト |
| RAM | 768 バイト | 981 バイト（スタック 256 を含む） | 1,000 バイト（同） |

- **hlang 版（02）は、アセンブラ版より平均で 8% ほど速い。** 1/60 秒を超えるフレームも、アセンブラ版の半分ほど。調整なしの 01 でも、アセンブラ版とほぼ同じ速さ。SDCC 版は、敵の撃ち返しが増える場面で 1/60 秒に収まらず、ゲームが遅くなる。
- 書き方の調整（01 → 02）で縮むのは約 8%。コンパイラを直してきた経緯と、残っている点は [compiler-todo.md](compiler-todo.md) にある。

## アセンブラ版との一致

動きの基準は、元の手書きアセンブラ版（[../damaxian_asm](../damaxian_asm)）です。`../damaxian_c`（C 版）は、アセンブラ版を C に書き直したもので、動きが少し変わっている箇所があり、hlang 版はそこをアセンブラ版に合わせてあります。

C 版（SDCC）と違う箇所（hlang 版はアセンブラ版の動き）:

| 箇所 | C 版 | アセンブラ版（hlang 版） |
| --- | --- | --- |
| 敵の「イン」 | `count0 == 0` のとき `count0 -= 2`（待たずに動く） | `count0 != 0` の間は `count0 -= 2` で待ち、0 になってから動く |
| 敵の絵（種類 0 の色） | `0x0a` と `0x09` を交互 | 両方 `0x0a` |
| 弾が画面の外に出る判定 | `x > 0xc4 \|\| y > 0xc4` | `x >= 0xc4 \|\| y >= 0xc4` |
| 自機と弾の当たり判定 | 座標を `i8` にして差を取る（座標が 128 以上で外れる） | 符号なし 8 ビットの差の絶対値 |
| 撃ち返しを止める判定 | `a + 0x30` を丸めない | 8 ビットで丸める |
| タイトルの色アニメ | 式の評価順に任せる | 1 つ目の乱数で色、2 つ目で位置 |
| ターン → アプローチ、タイトル、ゲームの終わり | `phase` を戻さない | `phase = 0` に戻す（動きには影響しない） |

openMSX で、同じ乱数の種と同じ入力（45 フレームごとに左右、9 フレームに 1 回スペース）をアセンブラ版と hlang 版に与え、**12,500 フレーム（約 3 分半。タイトル、プレイ、タイムアップ、ゲームオーバー、ハイスコア更新の曲、タイトルへ戻るまで）を毎フレーム比べて、次がすべて一致**しました。

- スプライト表（32 枚）
- 敵 9 体と弾 18 個の状態（位置、角度、状態、カウント）
- モードと状態
- スコア、倍率、タイマ
- PSG の 14 レジスタ（サウンド）

測定は [tools/](tools/) と、保存した [versions/](versions/) にあります。
