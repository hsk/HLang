; bench/vm.hl と同じバイトコードマシンを手で書いたもの。スタックはメモリに置く。
;   DE = 読み取り位置、BC = スタックの一番上の番地（スタックは 1 ページに収まるので C だけ動かす）
        org 0x4000
        call run
        halt

run:    ld de,prog
        ld bc,0xe07f            ; 最初に積むのは 0xe080
loop:   ld a,(de)               ; 命令を読んで表で飛ぶ（37 T）
        inc de
        ld l,a
        ld h,table>>8
        ld a,(hl)
        inc h
        ld h,(hl)
        ld l,a
        jp (hl)

lit:    ld a,(de)
        inc de
        inc c
        ld (bc),a
        jp loop
add:    ld a,(bc)
        ld h,a
        dec c
        ld a,(bc)
        add a,h
        ld (bc),a
        jp loop
sub:    ld a,(bc)
        ld h,a
        dec c
        ld a,(bc)
        sub h
        ld (bc),a
        jp loop
dup:    ld a,(bc)
        inc c
        ld (bc),a
        jp loop
emit:   ld a,(bc)
        dec c
        call 0xa2
        jp loop
jnz:    ld a,(de)
        inc de
        ld l,a
        ld a,(bc)
        or a
        jr z,loop
        ld h,0
        ld de,prog
        add hl,de
        ex de,hl
        jp loop
dec:    ld a,(bc)
        dec a
        ld (bc),a
        jp loop
end:    ret

prog:   db 0,200
        db 0,65,3,0,1,1
        db 2,4
        db 6
        db 5,2
        db 7

        ds 0x4100-$             ; 表は 256 バイト境界に置く。下位のページの次が上位のページ
table:  db lit&255,add&255,sub&255,dup&255,emit&255,jnz&255,dec&255,end&255
        ds 0x4200-$
        db lit>>8,add>>8,sub>>8,dup>>8,emit>>8,jnz>>8,dec>>8,end>>8
