# SMS の ROM を MAME（マーク III）で起動し、200 フレーム後の画面を PNG に撮る。画面は開かない。
#   .\tools\sms-screen.ps1 examples\sms_game.sms [出力.png]
param([Parameter(Mandatory)][string]$Rom, [string]$Out)
$rom = (Resolve-Path $Rom).Path
if (-not $Out) { $Out = [IO.Path]::ChangeExtension($rom, '.png') }
$dir = Join-Path ([IO.Path]::GetTempPath()) ("hlsms." + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory $dir | Out-Null
@'
local frames = 0
emu.register_frame_done(function()
  frames = frames + 1
  if frames == 200 then manager.machine.video:snapshot() end
  if frames == 205 then manager.machine:exit() end
end)
'@ | Set-Content -Encoding ascii (Join-Path $dir 'shot.lua')
Push-Location $dir
try {
  mame sg1000m3 -cart $rom -window -video none -sound none -nothrottle -skip_gameinfo `
    -snapshot_directory snap -cfg_directory cfg -nvram_directory nv -autoboot_script shot.lua | Out-Null
  Copy-Item (Join-Path $dir 'snap\sg1000m3\0000.png') $Out
  $Out
} finally { Pop-Location; Remove-Item -Recurse -Force $dir }
