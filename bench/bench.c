/* bench.hl と同じ処理。SDCC でコンパイルして比べる。 */
typedef unsigned char u8;
typedef unsigned int u16;

void fill(u8 *p, u8 n, u8 v);
void copy(u8 *dst, u8 *src, u16 n);
u8 sum(u8 *p, u8 n);
u16 count(u8 *p, u8 *end);

void main(void) {
  fill((u8 *)0xc000, 200, 3);
  copy((u8 *)0xc100, (u8 *)0xc000, 200);
  *(u8 *)0xc200 = sum((u8 *)0xc100, 200);
  *(u16 *)0xc202 = count((u8 *)0xc000, (u8 *)0xc200);
  __asm
    halt
  __endasm;
}

void fill(u8 *p, u8 n, u8 v) {
  while (n != 0) { *p = v; p++; n--; }
}

void copy(u8 *dst, u8 *src, u16 n) {
  while (n != 0) { *dst = *src; dst++; src++; n--; }
}

u8 sum(u8 *p, u8 n) {
  u8 acc = 0;
  while (n != 0) { acc += *p; p++; n--; }
  return acc;
}

u16 count(u8 *p, u8 *end) {
  u16 c = 0;
  while (p < end) { if (*p != 0) c++; p++; }
  return c;
}
