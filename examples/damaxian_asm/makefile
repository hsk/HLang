# DAMAXIAN（アセンブラ版）。SDCC 付属の sdasz80 と sdld で、16KB の ROM カートリッジにする
#   make      bin/DAMAXIAN.rom を作る
#   make run  openMSX（C-BIOS_MSX1）で動かす。タイトルで SPACE を押すと始まる

SRCDIR = sources
OBJDIR = objects
BINDIR = bin
VPATH = $(SRCDIR):$(OBJDIR)

AS = sdasz80
ASFLAGS = -ls -I$(SRCDIR) -I.
LD = sdcc
# コードは ROM（0x4000 からヘッダ、0x4010 から）、変数は RAM の 0xc000 から
LDFLAGS = -mz80 --no-std-crt0 --nostdinc --nostdlib --code-loc 0x4010 --data-loc 0xc000

TARGET = DAMAXIAN
SRCS = crt0.s main.s System.s App.s Back.s Title.s Game.s GameShip.s GameShot.s GameEnemy.s GameBullet.s pattern.s bg.s
OBJS = $(SRCS:.s=.rel)

.SUFFIXES: .s .rel
.PHONY: all run clean

all: $(BINDIR)/$(TARGET).rom

.s.rel:
	$(AS) $(ASFLAGS) -o $(OBJDIR)/$@ $<

$(OBJS): | $(OBJDIR)

# ihx の 0x4000〜0x7fff を ROM にする
$(BINDIR)/$(TARGET).rom: $(OBJS) | $(BINDIR)
	$(LD) $(LDFLAGS) -o $(BINDIR)/$(TARGET).ihx $(addprefix $(OBJDIR)/,$(OBJS))
	makebin -s 32768 -o 16384 $(BINDIR)/$(TARGET).ihx $@

run: all
	openmsx -machine C-BIOS_MSX1 -cart $(BINDIR)/$(TARGET).rom

$(OBJDIR) $(BINDIR):
	mkdir -p $@

clean:
	rm -rf $(OBJDIR) $(BINDIR)

-include makefile.depend
