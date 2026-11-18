# サンプルをコンパイルして openMSX で動かす。
#   make game          game.hl を ROM にして openMSX で動かす（tracks, music, hello なども同じ）
#   make tracks.rom    ROM を作るだけ
#   make clean         ROM を消す
#   make tracks MACHINE=Panasonic_FS-A1   機種を変える（既定は BIOS の要らない C-BIOS_MSX1）

# ソースから作るときは dune でビルドした物、リリースの中（src/ がない）では隣の hlc を使う
SRC := $(wildcard ../src)
HLC = $(if $(SRC),../_build/default/bin/hlc.exe,../hlc)
MACHINE = C-BIOS_MSX1

.PHONY: all clean sms
all: $(patsubst %.hl,%.rom,$(filter-out sms_%,$(wildcard *.hl))) sms_game.sms

# コンパイラは、ソースが変わったときだけ作り直す
ifneq ($(SRC),)
$(HLC): $(wildcard ../src/*.ml ../bin/*.ml ../emu/*.ml)
	cd .. && dune build bin/hlc.exe
endif

%.rom: %.hl $(HLC) ../lib/msx.hl
	$(HLC) $<

# セガ・マスターシステム版: make sms_game.sms で ROM、make sms で MAME（ウィンドウ）で動かす
%.sms: %.hl $(HLC) ../lib/sms.hl
	$(HLC) $< -sms

sms: sms_game.sms
	mame sg1000m3 -cart $< -window -skip_gameinfo

%: %.rom
	openmsx -machine $(MACHINE) -cart $<

clean:
	rm -f *.rom *.sms *.png

.PRECIOUS: %.rom
