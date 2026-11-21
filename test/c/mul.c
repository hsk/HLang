/* 変数どうしの掛け算と、固定小数点の掛け算 fmul / fmulu */
#include "out.h"
#ifndef __HLCC__
#include <stdint.h>
static i16 fmul(i16 a, i16 b, u8 n) { return (i16)(((int32_t)a * b) >> n); }
static u16 fmulu(u16 a, u16 b, u8 n) { return (u16)(((uint32_t)a * b) >> n); }
#endif

u8 g8 = 200;
i16 gs = -300;
u16 tab[4] = { 3, 5, 7, 11 };

static i16 sq(i16 x) { return x * x; }

void main(void) {
    u8 a = g8, b = 7;
    outh(a * b);                    /* int で掛ける: 1400 */
    u8 c = a * b;                   /* u8 に丸める */
    outh(c);
    i16 s = gs, t = 123;
    outh((u16)(s * t));             /* 下の 16 ビット */
    outh((u16)sq(s / 3));
    u16 w = 50000, v = 3;
    u16 p = w * v;
    outh(p);
    u16 x = 1;
    for (u8 i = 0; i < 4; i++) x = x * tab[i];
    outh(x);
    outh((u16)fmul(s, t, 8));       /* 32 ビットの積を右へずらす */
    outh((u16)fmul(16384, s, 14));
    outh(fmulu(w, w, 16));
    outh((u16)fmul(-32768, -32768, 16));
    out('\n');
}
