#!/usr/bin/env python3
"""openMSX を録画しながら動かし、閉じたら Twitter に上げられる MP4（H.264、yuv420p、音なし）にする。"""
import os, shlex, subprocess, sys
from pathlib import Path

ffmpeg, ffprobe = os.environ["FFMPEG"], os.environ["FFPROBE"]
rom, machine = os.environ["ROM"], os.environ["MACHINE"]
openmsx = [os.path.expanduser(a) for a in shlex.split(os.environ["OPENMSX"])]
vdir = Path(os.environ.get("OPENMSX_VIDEO_DIR") or Path.home() / ".openMSX/videos").expanduser()
vdir.mkdir(parents=True, exist_ok=True)
before = {p: p.stat().st_mtime_ns for p in vdir.glob("fire*.avi")}

print("録画中です。openMSX のウィンドウで遊んで、閉じると MP4 を作ります。", flush=True)
subprocess.run(openmsx + ["-machine", machine, "-ext", "geo3d", "-cart", rom,
                          "-script", str(Path(__file__).with_name("rec.tcl"))])

new = [p for p in vdir.glob("fire*.avi") if before.get(p) != p.stat().st_mtime_ns]
if not new:
    sys.exit(f"{vdir} に AVI がありません。MP4 は作りません")
avi = max(new, key=lambda p: p.stat().st_mtime_ns)
if avi.stat().st_size == 0:
    sys.exit(f"AVI が空です: {avi}")

def duration(f):
    return float(subprocess.check_output([ffprobe, "-v", "error", "-show_entries", "format=duration",
                                          "-of", "default=nw=1:nk=1", str(f)], text=True))

out, n = Path("fire.mp4"), 2
while out.exists():
    out, n = Path(f"fire_{n}.mp4"), n + 1
# 先頭 5 秒（起動の黒い画面）を切る。画面の外の枠を切って 768x636（偶数）にする
ss = ["-ss", "5"] if duration(avi) > 5 else []
subprocess.run([ffmpeg, "-y", *ss, "-i", str(avi), "-vf", "crop=768:636:96:42",
                "-c:v", "libx264", "-pix_fmt", "yuv420p", "-profile:v", "high", "-crf", "18",
                "-r", "30", "-an", "-movflags", "+faststart", str(out)], check=True)
avi.unlink()
d = duration(out)
print(f"作りました: {out}（{out.stat().st_size:,} バイト、{d:.1f} 秒）")
if d > 140:
    print("注意: Twitter は 140 秒までです。短く切ってください")
