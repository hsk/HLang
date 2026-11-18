/* インラインアセンブラ（SDCC の書き方）。clang では動かないので、asm.out は手で書いた */
#include "out.h"

#define IFNZERO(label) \
    jr z, label \

#define LDA(dst, src) \
    ld a, src \
    ld dst, a \

u8 counter;
u8 table[4] = { 10, 20, 30, 40 };
static const u8 bias = 3;

/* 1 つ目の 8 ビットの引数は A、戻り値も A */
u8 twice(u8 a) __naked {
    a;
    __asm
        add a, a
        ret
    __endasm;
}

/* 16 ビットが 1 つ目なら HL、2 つ目の 8 ビットは E */
u8 pick(u8 *hl, u8 e) __naked {
    hl; e;
    __asm
        ld d, #0
        add hl, de
        ld a, (hl)
        ret
    __endasm;
}

/* 8 ビットが 1 つ目なら、2 つ目の 8 ビットは L */
u8 sub2(u8 a, u8 l) __naked {
    a; l;
    __asm
        sub a, l
        ret
    __endasm;
}

/* 数のラベル、マクロ、#(式)、文字定数、d(ix)、_ の付いた C の名前 */
u8 digit(u8 a) __naked {
    a;
    __asm
        cp #'0
        jr c, 1$
        cp #('9+1)
        jr nc, 1$
        sub #'0
        ret
    1$:
        ld a, #0xff
        ret
    __endasm;
}

u8 third(void) __naked {
    __asm
        ld ix, #_table
        LDA(b, 2(ix))
        ld a, (_bias)
        add a, b
        or a
        IFNZERO(9$)
        ld a, #1
    9$:
        ret
    __endasm;
}

/* アセンブラからアセンブラの関数を呼ぶ */
u8 four(void) __naked {
    __asm
        ld a, #2
        call _twice
        call _twice
        ret
    __endasm;
}

void main(void) {
    outh(twice(21));
    outh(pick(table, 3));
    outh(sub2(50, 8));
    outh(digit('7')); outh(digit('x'));
    outh(third());
    outh(four());
    u8 keep = 5;
    counter = 1;
    __asm
        ld a, (_counter)
        add a, #10
        ld (_counter), a
        ld bc, #0
        ld de, #0
        ld hl, #0
    __endasm;
    outh(counter + keep);
    out('\n');
}
