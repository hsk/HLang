/* 符号付き 8 ビットの定数を 16 ビットへ広げる（(i8)0x81 は -127） */
#include "out.h"
u8 bytes[3] = { 0x7f, 0x80, 0xff };
void main(void) {
    i8 c = (i8)0x81;
    outh((u16)((i16)(i8)bytes[0] - (i8)0x81));
    outh((u16)((i16)(i8)bytes[0] - c));
    outh((u16)((i16)bytes[0] - (i8)0x81));
    outh((u16)((i8)bytes[0] - (i8)0x81));
    outh((u16)((i8)bytes[0] - c));
    outh((u16)(bytes[0] - (i8)0x81));
}
