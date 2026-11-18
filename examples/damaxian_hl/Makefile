# ソースから作るときは dune でビルドした物、リリースの中（src/ がない）では ../../hlc を使う
SRC := $(wildcard ../../src)
HLC = $(if $(SRC),../../_build/default/bin/hlc.exe,../../hlc)
MACHINE = C-BIOS_MSX1
export HLANG_LIB = $(abspath ../../lib)
SOURCES = $(wildcard *.hl)

.PHONY: all run clean assets

all: main.rom

# pattern.hl と bg.hl は、元の pattern.c と bg.c から作る
assets: pattern.hl bg.hl

pattern.hl bg.hl: tools/import_assets.py ../damaxian_c/sources/pattern.c ../damaxian_c/sources/bg.c
	python3 tools/import_assets.py

main.rom: $(SOURCES) ../../lib/msx.hl $(HLC)
	$(HLC) main.hl

ifneq ($(SRC),)
$(HLC): ../../src/*.ml ../../bin/*.ml ../../emu/*.ml
	cd ../.. && dune build bin/hlc.exe
endif

run: main.rom
	openmsx -machine $(MACHINE) -cart main.rom

clean:
	rm -f main.rom
