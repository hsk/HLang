# FIRE PLANET（V9968 + geo3d）。make で fire.rom、make run で openMSX
# openMSX は V9968 と geo3d のあるもの。OPENMSX=... で場所を指す
HLANG	= ../..
SRC	:= $(wildcard $(HLANG)/src)
HLCC	= $(if $(SRC),$(HLANG)/_build/default/bin/hlcc.exe,$(HLANG)/hlcc)
OPENMSX	?= openmsx
MACHINE	?= C-BIOS_V9968_JP
FFMPEG	?= /opt/homebrew/bin/ffmpeg
FFPROBE	?= /opt/homebrew/bin/ffprobe
# make R800=1: 掛け算に R800 の命令を使い、R800 のマシンで動かす
ifdef R800
FLAGS	= -r800
MACHINE	= C-BIOS_V9968_R800_JP
endif

all: fire.rom

tables.h: gen.py
	python3 gen.py > tables.h

fire.rom: main.c tables.h FORCE
	$(if $(SRC),cd $(HLANG); dune build bin/hlcc.exe)
	$(HLCC) $(FLAGS) main.c
	mv main.rom fire.rom

run: fire.rom
	$(OPENMSX) -machine $(MACHINE) -ext geo3d -cart fire.rom

# openMSX で遊びながら録画し、閉じると fire.mp4（Twitter に上げられる形）を作る。要 ffmpeg
recrun: fire.rom
	@command -v "$(FFMPEG)" >/dev/null || { echo "ffmpeg が要ります: brew install ffmpeg" >&2; exit 1; }
	FFMPEG="$(FFMPEG)" FFPROBE="$(FFPROBE)" ROM=fire.rom MACHINE="$(MACHINE)" OPENMSX="$(OPENMSX)" python3 tools/recrun.py

clean:
	rm -f fire.rom main.rom

.PHONY: all run recrun clean FORCE
