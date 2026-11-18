/* 制御: for、while、do-while、continue、break、switch、?:、&&、|| */
#include "out.h"

u8 count;
u8 bump(void) { count++; return count; }

void main(void) {
    for (u8 i = 0; i < 10; i++) {
        if (i == 3) continue;
        if (i == 7) break;
        out('0' + i);
    }
    out(' ');
    u8 j = 0;
    do {
        j++;
        if (j == 2) continue;
        out('a' + j);
    } while (j < 5);
    out(' ');
    u16 k = 0;
    for (k = 1000; k > 300; k -= 300) out('k');
    outh(k);
    u8 w = 5;
    while (w--) out('w');
    out(' ');
    for (u8 v = 0; v < 6; v++) {
        switch (v) {
        case 0: out('A'); break;
        case 1:
        case 2: out('B'); break;
        case 4: out('D'); break;
        default: out('X'); break;
        }
    }
    out(' ');
    i16 big = 300;
    switch (big) {
    case 300: out('3'); break;
    case 1: out('1'); break;
    }
    u8 t = 7;
    out(t > 5 ? 'T' : 'F');
    count = 0;
    if (t > 10 && bump() > 0) out('?');
    if (t > 1 || bump() > 0) out('o');
    if (t > 1 && bump() > 0) out('p');
    outh(count);
    u8 z = (bump(), bump());
    outh(z);
    u8 r = !t;
    outh(r + (t == 7));
    out('\n');
}
