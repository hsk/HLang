#!/bin/sh
# 同じバイトコードマシンを hlang と手書きアセンブリで作り、エミュレータで走らせて比べる。
#   bench/vm.sh        （リポジトリの先頭で dune build を済ませてから。z80asm が要る）
set -e
cd "$(dirname "$0")/.."
tmp=$(mktemp -d /tmp/hlvm.XXXXXX)
hlc=_build/default/bin/hlc.exe
run=_build/default/test/runbin.exe

for f in vm vm_ptr vm_top; do
  size=$($hlc bench/$f.hl | sed 's/.*: \([0-9]*\) .*/\1/')
  head -c "$size" bench/$f.rom > "$tmp/hl.bin"
  echo "hlang $f.hl: $($run "$tmp/hl.bin" 0x4000 0x4010)"
  $hlc bench/$f.hl --asm | awk '/^; fn run/,/^; fn main/' | grep -c '^    ' | sed 's/^/  run の命令数: /'
  rm -f bench/$f.rom
done
for f in vm vm_sp; do
  z80asm -o "$tmp/a.bin" bench/$f.asm
  echo "手書き $f.asm: $($run "$tmp/a.bin" 0x4000)"
done
rm -rf "$tmp"
