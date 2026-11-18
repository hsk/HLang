/* 整数の格上げ、符号、シフト、割り算 */
#include "out.h"

u8 g8 = 200;
i8 gs = -5;
u16 g16 = 1000;

void main(void) {
    u8 a = g8, b = 100;
    u16 s = a + b;              /* 300: int で足す */
    u8 c = a + b;               /* 44: u8 に丸める */
    outh(s); outh(c);
    if (a + b > 255) out('Y'); else out('N');
    i8 x = gs;
    x >>= 1;                    /* 算術シフト */
    outh((u16)(i16)x);
    u8 y = (u8)(a + 0x40);
    outh(y);
    i8 n = -1;
    if (n < 1) out('L'); else out('G');
    u8 m = 255;
    if (m == 255) out('E');
    if ((u8)(m + 1) == 0) out('Z');
    outh(g16 / 7); outh(g16 % 7); outh(g16 * 3);
    i16 neg = -1000;
    outh((u16)(neg / 7)); outh((u16)(neg % 7));
    outh((u16)(neg >> 3));
    outh(~a & 0xff); outh((u16)-a);
    u16 big = 0xfff0;
    big += 0x20;
    outh(big);
    outh(a << 2); outh((u8)(a << 2));
    outh(a ^ b | 3);
    out('\n');
}
