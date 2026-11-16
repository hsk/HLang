; bench/vm.hl と同じバイトコードマシンを手で書いたもの。CPU のスタックを使う。
;   DE = 読み取り位置。値は push af / pop af で積み下ろしする（1 個 2 バイト）
        org 0x4000
        call run
        halt

run:    ld (0xe000),sp          ; 終わるときに戻すため
        ld de,prog
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
        push af
        jp loop
add:    pop hl
        pop af
        add a,h
        push af
        jp loop
sub:    pop hl
        pop af
        sub h
        push af
        jp loop
dup:    pop af
        push af
        push af
        jp loop
emit:   pop af
        call 0xa2
        jp loop
jnz:    ld a,(de)
        inc de
        ld l,a
        pop af
        push af
        or a
        jr z,loop
        ld h,0
        ld de,prog
        add hl,de
        ex de,hl
        jp loop
dec:    pop af
        dec a
        push af
        jp loop
end:    ld sp,(0xe000)
        ret

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
