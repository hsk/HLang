/* 構造体、無名の共用体（固定小数点）、配列、ポインタ、表 */
#include "out.h"

#define N 4
#define SQ(x) ((x) * (x))

typedef struct {
    u8 state;
    union {
        struct { u8 xd; u8 xi; };
        i16 x;
    };
    i16 sp;
} OBJ;

OBJ objs[N];
static const u8 table[] = { 1, 2, 3, 5, 8, 13 };
static const u16 words[] = { 0x1234, 0xabcd };
static const u8 msg[] = "AB" "CD";
static const u8 *const ptrs[] = { msg, table };
u8 buf[8] = { 9, 8, 7 };

enum { RED, GREEN = 5, BLUE };

void move(OBJ *o) {
    o->x += o->sp;
    if (o->xi >= 0x80) o->state = 0;
}

u16 sum(const u8 *p, u8 n) {
    u16 s = 0;
    while (n--) s += *p++;
    return s;
}

void main(void) {
    for (u8 i = 0; i < N; i++) {
        objs[i].state = 1;
        objs[i].x = i << 8;
        objs[i].sp = 0x180 * (i + 1);
    }
    for (u8 t = 0; t < 30; t++)
        for (u8 i = 0; i < N; i++) move(&objs[i]);
    for (u8 i = 0; i < N; i++) { outh(objs[i].state); outh(objs[i].xi); outh(objs[i].xd); }
    out(' ');
    outh(sum(table, sizeof(table)));
    outh(words[1]); outh(msg[3]); outh(ptrs[1][4]); outh(*ptrs[0]);
    outh(buf[0] + buf[2] + buf[7]);
    outh(GREEN + BLUE + RED);
    outh(SQ(3 + 1));
    outh(sizeof(OBJ)); outh(sizeof(objs));
    OBJ copy;
    copy = objs[1];
    copy.state = 7;
    outh(copy.state + objs[1].state);
    u8 *p = buf;
    p += 2;
    outh(*p); outh(p - buf);
    u8 local[3];
    local[0] = 4; local[1] = 5; local[2] = 6;
    outh(sum(local, 3));
    u8 v = 3;
    u8 *q = &v;
    *q = 9;
    outh(v);
    out('\n');
}
