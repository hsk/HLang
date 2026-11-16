# サンプルをコンパイルして openMSX で動かす。
#   make game          game.hl を ROM にして openMSX で動かす（tracks, music, hello なども同じ）
#   make tracks.rom    ROM を作るだけ
#   make clean         ROM を消す
#   make tracks MACHINE=Panasonic_FS-A1   機種を変える（既定は BIOS の要らない C-BIOS_MSX1）

HLC = ../_build/default/bin/hlc.exe
MACHINE = C-BIOS_MSX1

.PHONY: all clean
all: $(patsubst %.hl,%.rom,$(wildcard *.hl))

# コンパイラは、ソースが変わったときだけ作り直す
$(HLC): $(wildcard ../src/*.ml ../bin/*.ml ../emu/*.ml)
	cd .. && dune build bin/hlc.exe

%.rom: %.hl $(HLC) ../lib/msx.hl
	$(HLC) $<

%: %.rom
	openmsx -machine $(MACHINE) -cart $<

clean:
	rm -f *.rom

.PRECIOUS: %.rom
