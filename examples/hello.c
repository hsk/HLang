/* hlcc（C フロントエンド）の例。hlang のライブラリ（msx.hl）の関数を C から呼ぶ。
     dune exec bin/hlcc.exe -- examples/hello.c --run */
#include "msx.hl"
#include <stdint.h>

typedef uint8_t u8;

static const u8 msg[] = "HELLO FROM C";

static void puts_(const u8 *s) {
    while (*s) chput(*s++);
}

static void putd(u8 v) {
    if (v >= 100) chput('0' + v / 100);
    if (v >= 10) chput('0' + v / 10 % 10);
    chput('0' + v % 10);
}

void main(void) {
    puts_(msg);
    chput(' ');
    u8 sum = 0;
    for (u8 i = 1; i <= 10; i++) sum += i;
    putd(sum);
}
