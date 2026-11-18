/* static な局所変数、#if、大域のポインタ、引数の番地、16bit の比較、代入の右辺を先に計算する */
#include "out.h"

#define LEVEL 2
#if LEVEL > 1 && defined(LEVEL)
#define NAME 'Y'
#else
#define NAME 'N'
#endif

u8 hist[4];
u8 *cursor = hist;

u8 tick(void) {
    static u8 n = 10;
    n++;
    return n;
}

void put(u8 v) { *cursor++ = v; }

void swap(u16 *a, u16 *b) { u16 t = *a; *a = *b; *b = t; }

i16 clamp(i16 v, i16 lo, i16 hi) { return v < lo ? lo : v > hi ? hi : v; }

void main(void) {
    out(NAME);
    outh(tick()); outh(tick());
    put(1); put(2); put(3);
    outh(hist[0] + hist[1] * 10 + hist[2] * 100);
    outh(cursor - hist);
    u16 a = 1, b = 2;
    swap(&a, &b);
    outh(a); outh(b);
    outh((u16)clamp(-500, -100, 100)); outh((u16)clamp(50, -100, 100)); outh((u16)clamp(5000, -100, 100));
    u16 x = 40000;
    if (x > 30000) out('>');
    i16 y = -30000;
    if (y < 100) out('<');
    u8 bits = 0x5a;
    u8 cnt = 0;
    for (u8 i = 0; i < 8; i++) if (bits & (1 << i)) cnt++;
    outh(cnt);
    hist[tick() & 3] = tick();   /* 右辺が先（13）、添字が後（14 & 3 = 2） */
    outh(hist[1]); outh(hist[2]);
    out('\n');
}
