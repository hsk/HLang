# examples の ROM を全部作る（Windows の PowerShell 用。Makefile の代わり）。
#   .\examples\build.ps1
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
Push-Location $root
try {
  dune build bin/hlc.exe
  $hlc = Join-Path $root '_build\default\bin\hlc.exe'
  Get-ChildItem examples\*.hl | ForEach-Object {
    if ($_.Name -like 'sms_*') { & $hlc $_.FullName -sms } else { & $hlc $_.FullName }
  }
} finally { Pop-Location }
