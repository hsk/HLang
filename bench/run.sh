#!/bin/sh
# hlang と SDCC で同じ処理をコンパイルし、エミュレータで走らせて時間と大きさを比べる。
#   bench/run.sh        （リポジトリの先頭で dune build を済ませてから）
set -e
cd "$(dirname "$0")/.."
tmp=$(mktemp -d /tmp/hlbench.XXXXXX)
hlc=_build/default/bin/hlc.exe
run=_build/default/test/runbin.exe

size=$($hlc bench/bench.hl | sed 's/.*: \([0-9]*\) .*/\1/')
head -c "$size" bench/bench.rom > "$tmp/hl.bin"
# ROM の先頭 16 バイトはヘッダで、プログラムは 0x4010 から始まる
echo "hlang                : $size バイト（ヘッダ 16 バイトを含む）  $($run "$tmp/hl.bin" 0x4000 0x4010)"

for opt in --opt-code-speed --opt-code-size; do
  sdcc -mz80 $opt --no-std-crt0 --code-loc 0x4000 --data-loc 0xc800 -o "$tmp/c.ihx" bench/bench.c 2> /dev/null
  sdobjcopy -I ihex -O binary "$tmp/c.ihx" "$tmp/c.bin"
  echo "sdcc $opt: $(wc -c < "$tmp/c.bin" | tr -d ' ') バイト  $($run "$tmp/c.bin" 0x4000)"
done
rm -rf "$tmp" bench/bench.rom
