# C フロントエンドのテスト

`*.c` を hlcc でコンパイルし、参照インタプリタとエミュレータで走らせた出力を、`*.out` と比べます（`test/t_c.ml`）。

`*.out` は、同じ C を clang で走らせた出力です。テストを足したら、次で作ります（clang の int は 32 ビットなので、16 ビットを超える計算は書かない。-fpack-struct=1 で、SDCC と同じく構造体に詰め物を入れない）。

```sh
cd test/c
clang -w -funsigned-char -fpack-struct=1 -o /tmp/t arith.c && /tmp/t > arith.out
```
