#!/bin/sh
# ROM を openMSX (C-BIOS) で起動し、数秒後の画面の文字を表示する。画面は開かない。
#   tools/msx-screen.sh examples/hello.rom
rom=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
tcl=$(mktemp /tmp/hlmsx.XXXXXX)
cat > "$tcl" <<'TCL'
set renderer none
set throttle off
after time 6 {
  for {set row 0} {$row < 24} {incr row} {
    set out ""
    for {set col 0} {$col < 32} {incr col} {
      set c [vpeek [expr {0x1800 + $row * 32 + $col}]]
      if {$c >= 32 && $c < 127} {append out [format %c $c]} else {append out " "}
    }
    puts stderr [string trimright $out]
  }
  exit
}
TCL
openmsx -machine C-BIOS_MSX1 -cart "$rom" -script "$tcl" 2>&1 | grep -v '^warning' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}'
rm -f "$tcl"
