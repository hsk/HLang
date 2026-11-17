#!/bin/sh
# SMS の ROM を MAME（マーク III。BIOS が要らない）で起動し、200 フレーム後の画面を PNG に撮る。画面は開かない。
#   tools/sms-screen.sh examples/sms_game.sms [出力.png]
rom=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
out=${2:-${rom%.sms}.png}
dir=$(mktemp -d /tmp/hlsms.XXXXXX)
cat > "$dir/shot.lua" <<'LUA'
local frames = 0
emu.register_frame_done(function()
  frames = frames + 1
  if frames == 200 then manager.machine.video:snapshot() end
  if frames == 205 then manager.machine:exit() end
end)
LUA
(cd "$dir" && mame sg1000m3 -cart "$rom" -window -video none -sound none -nothrottle -skip_gameinfo \
  -snapshot_directory snap -cfg_directory cfg -nvram_directory nv -autoboot_script shot.lua >/dev/null 2>&1)
cp "$dir/snap/sg1000m3/0000.png" "$out" && echo "$out"
rm -rf "$dir"
