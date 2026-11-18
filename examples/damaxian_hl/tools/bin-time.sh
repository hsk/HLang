#!/bin/sh
# 使い方: tools/bin-time.sh asm|sdcc|hlcc
# 比べる相手を、hlang 版と同じ測り方（tools/handler-time.sh）で測る。
#   asm:  アセンブラ版（../damaxian_asm）
#   sdcc: C 版を SDCC でビルドしたもの（../damaxian_c）
#   hlcc: C 版を hlcc でビルドしたもの（../damaxian_c の make rom2）
# どれも 16KB の ROM カートリッジにして、C-BIOS で測る。asm と sdcc は make で bin/DAMAXIAN.rom を作り、
# 変数の番地を、ビルドの .sym（モジュールの中の位置）と、リンク順に足した _DATA の大きさから求める
# （static の変数は map に出ないため）。
# 要る物: openMSX、python3、SDCC（4.6.0 で確かめた）。
HERE=$(cd "$(dirname "$0")" && pwd)
case "$1" in
  asm)  DIR=$HERE/../../damaxian_asm
        VARS="System:_flag System:_input System:random Back:count" ;;
  sdcc) DIR=$HERE/../../damaxian_c
        VARS="System:_flag System:_input Math:_random Back:_count" ;;
  hlcc) # C 版を hlcc（hlang の C フロントエンド）で ROM にする。番地は hlcc --asm の equ から。
        # 起動の長さが違うので、タイトルの数え（Title.c の count）を hlang 版と同じ 1 にそろえる
        DIR=$(cd "$HERE/../../damaxian_c" && pwd)
        (cd "$DIR" && make rom2 >/dev/null 2>&1) || { echo "hlcc のビルドに失敗"; exit 1; }
        SRCS="main System Sound Math App Back Game Ship Shot Enemy Bullet Title pattern bg"   # makefile の CSRCS と同じ順
        ASM=$(cd "$DIR/sources" && ../../../_build/default/bin/hlcc.exe --asm $(for f in $SRCS; do echo $f.c; done) 2>&1)
        eq() { echo "$ASM" | sed -n "s/^$1: equ \(0x[0-9a-f]*\)/\1/p"; }
        # count.5 は Back.c、count.11 は Title.c の static（数字はファイルの順番）
        ROM=$DIR/bin/DAMAXIAN2.rom BCNT=$(eq count.5) TCNT=$(eq count.11) RND=$(eq random) INP=$(eq input) FLAG=$(eq flag)
        export ROM BCNT TCNT RND INP FLAG
        exec sh "$HERE/handler-time.sh" ;;
  *) echo "使い方: $0 asm|sdcc|hlcc"; exit 1 ;;
esac
DIR=$(cd "$DIR" && pwd)
(cd "$DIR" && make >/dev/null 2>&1) || { echo "$DIR のビルドに失敗"; exit 1; }
set -- $(python3 - "$DIR" $VARS <<'PY'
import re, sys
d = sys.argv[1]
mods = [l.strip()[len("objects/"):-4] for l in open(f"{d}/bin/DAMAXIAN.lk") if l.strip().endswith(".rel")]
base = int(re.search(r"([0-9A-F]+)\s+s__DATA\b", open(f"{d}/bin/DAMAXIAN.map").read()).group(1), 16)
addr = {}
for m in mods:                       # モジュールごとの _DATA の先頭番地（リンク順に詰めて置かれる）
    t = open(f"{d}/objects/{m}.sym", encoding="latin-1").read()
    a = re.search(r"^\s*(\d+) _DATA\s+size\s+([0-9A-Fa-f]+)", t, re.M)
    area, size = a.groups() if a else ("-", "0")   # _DATA のないモジュールもある
    addr[m] = (base, area, t)
    base += int(size, 16)
for v in sys.argv[2:]:
    m, name = v.split(":")
    b, area, t = addr[m]
    off = re.search(rf"(?:^|\|)\s*{area} {re.escape(name)}\s+([0-9A-Fa-f]+)", t, re.M).group(1)
    print(hex(b + int(off, 16)))
PY
)
[ $# -eq 4 ] || { echo "番地が求められない"; exit 1; }
FLAG=$1 INP=$2 RND=$3 BCNT=$4 ROM=$DIR/bin/DAMAXIAN.rom
export ROM BCNT RND INP FLAG
sh "$HERE/handler-time.sh"
