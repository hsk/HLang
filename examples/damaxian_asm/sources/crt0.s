; crt0.s : ブートコード（16KB の ROM カートリッジ。0x4000 から）
    .module crt0
    .include    "bios.inc"
; 外部関数宣言
    .globl  _main
    .globl  s__DATA, l__DATA
; HEADER 領域（ROM のヘッダ。0x4000〜0x400f）
    .area   _HEADER (ABS)
    .org    0x4000
    .db     0x41, 0x42      ; "AB"
    .dw     _boot           ; INIT
    .dw     0, 0, 0, 0, 0, 0
; CODE 領域の先頭（crt0 は最初にリンクするので 0x4010）
    .area   _CODE
_boot: ; ブートコード
    ; RAM（DATA 領域）を 0 で埋める
        ld      hl, #s__DATA
        ld      de, #s__DATA + 1
        ld      bc, #l__DATA - 1
        ld      (hl), #0
        ldir
    ; BIOS のスタックを残して、自分のスタックへ
        ld      (savedsp), sp
        ld      sp, #stackfinal
        call    gsinit
    ; main() 関数の呼び出し
        call    _main
_terminated:: ; 終了（BIOS のスタックに戻して、BIOS へ返る）
        ld      sp, (savedsp)
        ret
    .area   _GSINIT
gsinit: ; 変数の初期化
    ; GSFINAL 領域
    .area   _GSFINAL
gsfinal: ; 変数の初期化の完了
    ret
    .area   _DATA
data:; DATA 領域の開始
savedsp: .ds   2   ; BIOS のスタック
stack: .ds     256 ; スタック領域
stackfinal: ; DATA 領域の末端
    .area   _DATAFINAL
datafinal: ; DATA 領域の終了
