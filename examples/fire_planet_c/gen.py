# tables.h を作る: python3 gen.py > tables.h
# 絵の作り方と、三角関数、アーチの節の表。
import math

N = 256    # 1 周の角度の数
NN = 16    # アーチのマス数（節は NN + 1 個。main.c の ARC と同じ）

def arr(t, name, xs):
    print(f"static const {t} {name}[{len(xs)}] = {{")
    for i in range(0, len(xs), 16):
        print("    " + ", ".join(str(x) for x in xs[i:i + 16]) + ",")
    print("};")

def ang(f, k):
    return [round(k * f(2 * math.pi * a / N)) for a in range(N)]

# 16 色。0 は透明（元の PALETTE_16）。MSX は 3 ビットなので丸める
P16 = [(0, 0, 0), (12, 0, 0), (36, 4, 0), (68, 8, 0), (104, 12, 0), (144, 20, 0), (184, 32, 0), (220, 52, 0),
       (248, 80, 0), (255, 116, 0), (255, 152, 16), (255, 188, 36), (255, 220, 72), (255, 244, 136),
       (255, 255, 208), (255, 255, 255)]
pal = [tuple(round(v * 7 / 255) for v in c) for c in P16]

def clamp(v, lo, hi):
    return max(lo, min(hi, v))

# 元の generateProminenceTextures（size を 32 に）。外側に芯を重ねた 1 枚（芯があれば芯、なければ外側）
def fireball(v, size):
    half = size / 2
    p1, p2 = v * 1.1, v * 2.3
    t = [[0] * size for _ in range(size)]
    for y in range(size):
        for x in range(size):
            dx, dy = (x - half + 0.5) / half, (y - half + 0.5) / half
            dist = math.hypot(dx, dy)
            a = math.atan2(dy, dx)
            n1 = math.sin(a * 5 + p1 + dist * 4.5) * 0.5 + 0.5
            n2 = math.cos(a * 9 - p2 + dist * 7.5) * 0.5 + 0.5
            cell = n1 * 0.6 + n2 * 0.4
            edge = dist + (cell - 0.5) * 0.24 * dist
            it = clamp(max(0, 1 - edge) ** 1.45 * (0.72 + 0.38 * cell), 0, 1)
            if edge < 0.96:
                t[y][x] = 7 if it >= 0.42 else 6 if it >= 0.30 else 5 if it >= 0.18 else 4 if it >= 0.08 else 3
            if it >= 0.44 + (1 - cell) * 0.10 and edge < 0.72:
                t[y][x] = next(c for th, c in ((0.90, 15), (0.82, 14), (0.74, 13), (0.66, 12), (0.58, 11), (0.50, 9), (0, 8)) if it >= th)
    return t

# 元の generateMagmaTextures の模様を、縦にも横にもつなげられるようにした 64 x 32 の 1 枚。穴（透明）はない。
# 横に 2 周期、縦に 2 周期（sin / cos の周期が絵の大きさにぴったり合う）
def lava(w=64, h=32):
    t = [[0] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            nx = 2 * math.pi * 2 * x / w
            ny = 2 * math.pi * 2 * y / h
            ridge = abs(math.sin(nx + math.cos(ny)) + math.cos(ny * 1.5 - nx * 0.5))
            crack = max(0, 1 - ridge * 0.85)
            t[y][x] = 12 if crack > 0.72 else 10 if crack > 0.58 else 8 if crack > 0.44 else 6 if crack > 0.30 else 4 if crack > 0.16 else 3
    return t

print("/* gen.py が作る。手で直さない */")
arr("i16", "COS", ang(math.cos, 16384))   # Q14
arr("i16", "SIN", ang(math.sin, 16384))
arr("u8", "PAL", [b for r, g, bl in pal for b in ((r << 4) | bl, g)])

# 火の玉 32 x 32 を 4 ビットにつめる（1 行 16 バイト）。VRAM の x = 0、y = 640（板の v = 128）に置く
fb = fireball(0, 32)
arr("u8", "FIRE", [(r[x] << 4) | r[x + 1] for r in fb for x in range(0, 32, 2)])
arr("u8", "UV", [0, 128, 31, 159])     # 火の玉の u0, v0, u1, v1
# 溶岩 64 x 32（1 ドット 1 バイト）。main.c が、これから 4 段のミップを作る
arr("u8", "LAVA", [c for r in lava() for c in r])

# アーチの節 j（t = j / NN）
t = [j / NN for j in range(NN + 1)]
arr("u8", "AN", [round(128 * (1 - x)) & 255 for x in t])        # 角 π(1 - t)
arr("u8", "WA", [round(x * 384) & 255 for x in t])              # うねり sin(3πt + index)
arr("u8", "WB", [round(x * 256) & 255 for x in t])              # うねり cos(2πt + index)
arr("u8", "TP", [round(x * 128) for x in t])                    # sin(π t)

# 空の帯（元の背景の段の間）。画面の行（地平線から）の境目と、帯の色
ny = [0.14, 0.33, 0.41, 0.46, 0.49, 0.51, 0.535, 0.585, 0.685, 0.875]
arr("i16", "BP", [-400] + [round(v * 512 - 266) for v in ny] + [400])
arr("u8", "BC", [1, 2, 3, 5, 7, 9, 11, 8, 5, 3, 2])
