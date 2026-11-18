/* テスト用の出力。hlcc では hlang の extern（エミュレータが受け取る）、clang では putchar */
#include <stdint.h>
typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
#ifdef __HLCC__
#include "out.hl"
#else
#include <stdio.h>
static void out(u8 c) { putchar(c); }
static void outh(u16 v) { printf("%04x ", v); }
#endif
