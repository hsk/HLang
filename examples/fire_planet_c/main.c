/* FIRE PLANET（V9968 + geo3d）。溶岩の海と炎のアーチ。
 * 溶岩の床、炎のアーチ、空の帯を、すべて geo3d の板（ビルボード）で描く。
 * 板は、毎フレーム Z80 が四隅を計算して送る。奥から順に並べるのも、画面の端で切るのも geo3d。
 * カメラは左右上下に動くだけ（前へは自動で進む）。ロールは行列で掛けるので、板も空の帯も一緒に傾く。
 *
 * 座標は元と同じ単位。y は上向きで、溶岩の面が 0（元は下向きで 220）。
 * 操作: カーソルキーで自分で飛ぶ、A でオートパイロットに戻る */
#include <stdint.h>
typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;

#ifndef __HLCC__
void out(u8 port, u8 v);
u8 inp(u8 port);
void outs(u8 port, u16 src, u8 n);
void di(void);
i16 fmul(i16 a, i16 b, u8 n);
u16 fmulu(u16 a, u16 b, u8 n);
#endif

#include "tables.h"

#define P0 0x98
#define P1 0x99
#define P2 0x9A
#define P4 0x9C
#define GI 0x9D
#define GD 0x9F

#define ZMAX 3200       /* 遠くの端 */
#define ZMIN 48         /* 手前で消す（元は 20。近いと板が画面より大きくなる） */
#define ZCUT 3072       /* 奥は描かない（元の ZMAX x 0.96） */
#define ZB 3375         /* 空の帯の奥行き（ZB / F = 25） */
#define FWD 26          /* 1 フレームに進む量（元の 780 / 30） */
#define LFWD 52         /* 溶岩が 1 フレームに手前へ来る量（アーチの 2 倍） */

/* 溶岩の床は、奥行きの輪切り NR 枚（板は横に 1 枚）。板は画面の横幅を覆い、奥ほど広くする（ミップ）。
   段 k の板は、半分の幅が HW0 x 2^k で、絵は「模様 1 個の幅を 1/2^k にして、256 の幅に 4 x 2^k 回繰り返した」もの。
   つまり模様の大きさは世界で同じ（300）のまま、板だけが 2 倍ずつ広がる。段は 4 つ（0〜3） */
#define NR 16                   /* 床の輪切りの数 */
#define DZ (ZMAX / NR)          /* 1 枚の奥行き（200） */
#define HW0 600                 /* 段 0 の半分の幅。z 460 まで、ロールしても画面の横を覆う */
#define NA 2                    /* アーチの数 */
/* アーチの粒は、輪（RS マス）の上を回る。輪のうち ARC マスぶんがアーチ（地面の上）で、残りは地面の下（描かない）。
   連なり（NP 個）が NC 個あって、輪の上を並んで進む。粒は足の 1 か所（噴火口）から出て、アーチを越えて、もう一方の足で地面に消える。
   連なりが地面の下にいる間は、アーチは空になる。RS 24、NC 1 なら、8 個の連なりが 1 つで、空白が 16 マス。RS 32、NC 2 なら、連なり 2 つと空白 2 つ */
#define ARC 16                  /* アーチのマス数（節は ARC + 1 個。gen.py の NN と同じ） */
#define NN (ARC + 1)
#define RS 24                   /* 輪のマス数 */
#define NC 1                    /* 連なりの数 */
#define NP 8                    /* 連なりの粒の数 */
#define NPT (NC * NP)           /* アーチ 1 本の粒の数 */
/* 面: 溶岩の床、アーチ（粒ごとに 1 枚）、空の帯 */
#define FA NR
#define FS (FA + NA * NPT)
#define FB FS
#define NBD 6                   /* 空の帯（色の面） */
#define NF (FB + NBD)

/* ---- VDP ---- */

static void vr(u8 r, u8 v) { out(P1, v); out(P1, 0x80 | r); }

static u8 st(u8 n) {
    vr(15, n);
    u8 v = inp(P1);
    vr(15, 0);
    return v;
}

static void vat(u16 y, u8 x) {
    vr(14, y >> 7);
    out(P1, ((y & 1) << 7) | (x >> 1));
    out(P1, 0x40 | ((y >> 1) & 0x3f));
}

static void screen(void) {
    vr(0, 0x06); vr(1, 0x40); vr(2, 0x1f); vr(7, 0x00); vr(8, 0x0a); vr(9, 0x80);
    vr(16, 0);
    outs(P2, (u16)PAL, 32);
    out(P4, 0x00);              /* ロックを外す */
    vr(21, 0x00);               /* V58 = 0: V9968 の拡張 */
    vr(20, 0x01);               /* コマンドを速く */
}

/* 絵は y = 512 から（geo3d の TEXY。板の v は 0〜255）。
 *   v 0〜127     溶岩のミップ 4 段（256 x 32 が 4 枚。段 k は v = 32 k から）
 *   v 128〜159   火の玉（32 x 32、x = 0） */
static u8 rb[128];

static void textures(void) {
    for (u8 k = 0; k < 4; k++)                          /* 溶岩: 段 k は、絵の x を 2^k 倍して読む（1 周期が 64 / 2^k ドットになり、256 に 4 x 2^k 回並ぶ） */
        for (u8 y = 0; y < 32; y++) {
            u16 row = (u16)y << 6;
            for (u8 i = 0; i < 128; i++) {
                u16 x = (u16)i << 1;
                rb[i] = (LAVA[row + ((x << k) & 63)] << 4) | LAVA[row + (((x + 1) << k) & 63)];
            }
            vat(512 + ((u16)k << 5) + y, 0);
            outs(P0, (u16)rb, 128);
        }
    for (u8 r = 0; r < 32; r++) { vat(640 + r, 0); outs(P0, (u16)(FIRE + (u16)r * 16), 16); }
}

/* ---- geo3d ---- */

static void gi(u8 i) { out(GI, i); }
static void gd(u8 v) { out(GD, v); }
static void gw(i16 v) { out(GD, v); out(GD, v >> 8); }

static void face(u16 a, u8 base) {
    u16 b = a + 1, c = a + 2, d = a + 3;
    gd(a); gd(b); gd(c); gd(d);
    gd(((a >> 8) & 3) | (((b >> 8) & 3) << 2) | (((c >> 8) & 3) << 4) | (((d >> 8) & 3) << 6));
    gw(0); gw(0); gw(0);
    gd(base);
}

static void tuv(u8 id) {
    u8 i = id << 2;
    gd(UV[i]); gd(UV[i + 1]); gd(UV[i + 2]); gd(UV[i + 1]);
    gd(UV[i + 2]); gd(UV[i + 3]); gd(UV[i]); gd(UV[i + 3]);
}

/* ---- 世界 ---- */

static u16 seed = 12345;
static u8 rn(u8 n) { seed = seed * 25173 + 13849; return ((seed >> 8) * n) >> 8; }

static i16 fz[NR];                                      /* 床の輪切りの手前の端の奥行き */
static u8 lvl[NR];                                      /* 板のミップの段 */
static i16 az[NA], acx[NA], ah[NA];                     /* アーチ */
static u16 fl[NA];                                      /* 輪の上の位置（1 マス 256）。連なりの先頭 */
static u8 asp[NA];                                      /* 流れる速さ（1 フレームに 1 マスの何 / 256） */
static i16 nx[NA * NN], ny[NA * NN], nz[NA * NN], nh[NA * NN];   /* 節: 中心からの横、高さ、奥行き、半分の大きさ */
static i16 camx = 0, camy = 180, tgx = 0, tgy = 180;   /* カメラ（y は溶岩から） */
static i16 roll16 = 0;                                  /* ロール（1 周 256 の 1/16） */
static u8 auto_ = 1, archn = 7;
static u16 t = 0;

/* 元の createProminenceArch。幅 420〜760、高さ 320〜540、太さ 85〜120 */
static void mkarch(u8 a, u8 index) {
    i16 aw = 420 + rn(255) + rn(85), ht = 320 + rn(220), th = 85 + rn(35);
    u8 ix = index * 53;
    i16 cx = fmul(SIN[ix], 260, 14) + rn(140) - 70;
    u8 tl = rn(19) - 9;                                 /* 傾き ±0.225 rad */
    i16 ct = COS[tl], sn = SIN[tl];
    acx[a] = cx; ah[a] = ht; fl[a] = rn(255) * (RS * 256 / 255); asp[a] = 96 + rn(64);
    i16 sl = 6000 + rn(255) * 16;                       /* 前後への傾き（Q14、0.37〜0.74）。向きは左右どちらか */
    if (rn(2)) sl = -sl;
    for (u8 j = 0; j < NN; j++) {
        u8 an = AN[j], ia = WA[j] + index * 41, ib = WB[j] + index * 41;
        i16 lx = fmul(COS[an], aw, 14) + fmul(SIN[ia], 35, 14);
        i16 ly = fmul(COS[ib], 25, 14) - fmul(SIN[an], ht, 14);
        i16 rx = fmul(lx, ct, 14) - fmul(ly, sn, 14);
        i16 ry = fmul(lx, sn, 14) + fmul(ly, ct, 14);
        i16 sh = fmul(th, 1500 + fmul(SIN[TP[j]], 14884, 14), 14);   /* 足（地面の下）では点になり、真ん中は太い。粒は地面の下から出て、下へ消える */
        u8 k = a * NN + j;
        nx[k] = rx; ny[k] = -90 - ry; nz[k] = fmul(rx, sl, 14);
        nh[k] = sh / 2;                                 /* 描く大きさの半分（元は 1.1 倍。粒が少ないので小さめに） */
    }
}

static void world(void) {
    for (u8 r = 0; r < NR; r++) fz[r] = (i16)r * DZ;
    for (u8 a = 0; a < NA; a++) { az[a] = 600 + a * (ZMAX / NA); mkarch(a, a); }
}

static void geo_init(void) {
    gi(0x6f); gd(1);                                    /* 拡張モード（10 ビットの番号） */
    gi(0x18); gw(135); gw(128); gw(106); gw(ZMIN); gw(256); gw(212);   /* F, CX, CY, ZNEAR, W, H */
    gi(0x40); gd(0); gd(0); gd(0); gd(0); gd(15); gd(8);               /* LOP = TIMP（0 は透明） */
    gi(0x68); gd(0); gd(0); gd(0); gd(0); gd(0); gd(0); gd(0);
    gi(0x58); gd(0); gd(0); gw(0); gw(0); gw(0);
    gi(0x52);
    for (u16 f = 0; f < NF; f++) face(f << 2, f >= FB ? BC[f - FB] : 0x80);
    gi(0x60); gw(0); gw(512); gd(0); gd(0);             /* TEXX, TEXY, TSTRIDE, TADDR */
    gi(0x53);
    for (u8 k = 0; k < NR; k++) { gd(0); gd(0); gd(255); gd(0); gd(255); gd(31); gd(0); gd(31); }   /* 床（毎フレーム段で書き直す） */
    for (u8 k = 0; k < NA * NPT; k++) tuv(0);
}

/* ---- 1 フレーム ---- */

static u8 qb[24];               /* 1 枚の頂点: 左上、右上、右下、左下 x (x, y, z) */

/* 左上 (x0, y0)、右下 (x1, y1)、奥行き z の板を送る。1 バイトずつ out すると遅いので、まとめて otir する */
static void quad4(i16 x0, i16 y0, i16 x1, i16 y1, i16 z) {
    qb[0] = x0; qb[1] = x0 >> 8; qb[2] = y0; qb[3] = y0 >> 8; qb[4] = z; qb[5] = z >> 8;
    qb[6] = x1; qb[7] = x1 >> 8; qb[8] = y0; qb[9] = y0 >> 8; qb[10] = z; qb[11] = z >> 8;
    qb[12] = x1; qb[13] = x1 >> 8; qb[14] = y1; qb[15] = y1 >> 8; qb[16] = z; qb[17] = z >> 8;
    qb[18] = x0; qb[19] = x0 >> 8; qb[20] = y1; qb[21] = y1 >> 8; qb[22] = z; qb[23] = z >> 8;
    outs(GD, (u16)qb, 24);
}

/* 床の輪切り: y = 0 の水平な四角。奥の辺 zf が左上と右上、手前の辺 zn が右下と左下。zf が見えない所なら、カメラの後ろへ置く */
static void floorq(i16 x0, i16 x1, i16 zn, i16 zf) {
    if (zf < ZMIN) { zn = -200; zf = -200; }
    else if (zn < ZMIN) zn = ZMIN;
    qb[0] = x0; qb[1] = x0 >> 8; qb[2] = 0; qb[3] = 0; qb[4] = zf; qb[5] = zf >> 8;
    qb[6] = x1; qb[7] = x1 >> 8; qb[8] = 0; qb[9] = 0; qb[10] = zf; qb[11] = zf >> 8;
    qb[12] = x1; qb[13] = x1 >> 8; qb[14] = 0; qb[15] = 0; qb[16] = zn; qb[17] = zn >> 8;
    qb[18] = x0; qb[19] = x0 >> 8; qb[20] = 0; qb[21] = 0; qb[22] = zn; qb[23] = zn >> 8;
    outs(GD, (u16)qb, 24);
}

/* 中心 (x, y, z)、半分の幅 w、半分の高さ h の板。近すぎる、遠すぎるものは、カメラの後ろへ置いて描かない */
static void quad(i16 x, i16 y, i16 z, i16 w, i16 h) {
    if (z < ZMIN || z > ZCUT) z = -200;
    quad4(x - w, y + h, x + w, y - h, z);
}

static u8 key(u8 r) {
    out(0xaa, (inp(0xaa) & 0xf0) | r);
    return inp(0xa9);
}

static i16 clamp(i16 v, i16 lo, i16 hi) { return v < lo ? lo : (v > hi ? hi : v); }

static void update(void) {
    u8 k8 = key(8);
    if (!(k8 & 0x10)) { tgx -= 15; auto_ = 0; }
    if (!(k8 & 0x80)) { tgx += 15; auto_ = 0; }
    if (!(k8 & 0x20)) { tgy += 12; auto_ = 0; }
    if (!(k8 & 0x40)) { tgy -= 12; auto_ = 0; }
    if (!(key(2) & 0x40)) auto_ = 1;

    /* 一番手前のアーチの門の真ん中をねらう */
    if (auto_) {
        i16 mz_ = 32000;
        u8 n = 0;
        for (u8 a = 0; a < NA; a++) if (az[a] > 80 && az[a] < mz_) { mz_ = az[a]; n = a; }
        tgx = acx[n] + fmul(SIN[(u8)(t * 2)], 60, 14);
        tgy = ((ah[n] * 23) >> 6) - fmul(COS[(u8)(t * 3)], 25, 14);
    }
    tgx = clamp(tgx, -550, 550);
    tgy = clamp(tgy, 75, 440);

    i16 px = camx;
    camx += ((tgx - camx) * 5) >> 5;
    camy += ((tgy - camy) * 5) >> 5;
    roll16 += ((-(camx - px) * 9 - roll16) * 3) >> 4;   /* 旋回でバンクする */

    for (u8 r = 0; r < NR; r++) {                       /* 床が手前へ来る。手前を過ぎた輪切りは、一番奥へ */
        fz[r] -= LFWD;
        if (fz[r] + DZ < ZMIN) fz[r] += ZMAX;
    }
    for (u8 a = 0; a < NA; a++) {
        az[a] -= FWD;
        fl[a] += asp[a];
        if (fl[a] >= RS * 256) fl[a] -= RS * 256;
        if (az[a] < 20) {
            i16 far = 0;
            for (u8 b = 0; b < NA; b++) if (az[b] > far) far = az[b];
            az[a] = far + ZMAX / NA;
            mkarch(a, archn++);
        }
    }
}

static void pass(u16 vf, u8 ctrl) {
    gi(0x42); gd(NF);
    gi(0x54); gd(vf); gd(vf >> 8);
    gi(0x48); gd(ctrl);
    while (inp(GI) & 1) {}
}

static void frame(u8 back) {
    gi(0x46); gd(0); gd(back);                          /* YPAGE: 裏のページに描く */
    for (u8 r = 0; r < NR; r++) {                       /* 床の板の段: 奥の辺で、画面の横を覆える一番細い段。uv を、その段の絵に */
        i16 zf = fz[r] + DZ;
        u8 lv = zf > 1850 ? 3 : (zf > 920 ? 2 : (zf > 460 ? 1 : 0));
        u8 v0 = lv << 5;
        lvl[r] = lv;
        gi(0x65); gd(r); gi(0x53);
        gd(0); gd(v0); gd(255); gd(v0); gd(255); gd(v0 + 31); gd(0); gd(v0 + 31);
    }
    gi(0x40); gd(0); gi(0x68); gd(0);                   /* VADDR = 0 */
    gi(0x50);                                           /* 頂点を、作りながら直に送る */
    for (u8 r = 0; r < NR; r++) {
        i16 w = HW0 << lvl[r];
        floorq(camx - w, camx + w, fz[r], fz[r] + DZ);
    }
    for (u8 a = 0; a < NA; a++)                         /* 粒が輪の上を進む。地面の下のマスは描かない */
        for (u8 c = 0; c < NC; c++)
            for (u8 p = 0; p < NP; p++) {
                u16 ru = fl[a] + (u16)((c * (RS / NC) + p) << 8);
                if (ru >= RS * 256) ru -= RS * 256;
                u8 j = ru >> 8;
                if (j >= ARC) { quad(0, 0, -200, 0, 0); continue; }
                i16 f = ru & 255;
                u8 k = a * NN + j;
                i16 x = acx[a] + nx[k] + fmul(nx[k + 1] - nx[k], f, 8);
                i16 y = ny[k] + fmul(ny[k + 1] - ny[k], f, 8);
                i16 z = az[a] + nz[k] + fmul(nz[k + 1] - nz[k], f, 8);
                i16 h = nh[k] + fmul(nh[k + 1] - nh[k], f, 8);
                quad(x, y, z, h, h);
            }

    /* 空の帯: 奥の ZB に、地平線（カメラの高さで上下する）の上へ。色の面 6 枚（下は地面が覆うので描かない） */
    i16 hor = (camy * 43) >> 10;
    for (u8 i = 0; i < NBD; i++)
        quad4(camx - 6000, camy - (BP[i] + hor) * 25, camx + 6000, camy - (BP[i + 1] + hor) * 25, ZB);

    /* カメラ: 回転はロールだけ。x' = c x + s y、y' = -s x + c y。T = -(行列 x カメラの位置) */
    u8 ra = roll16 >> 4;
    i16 c = COS[ra], s = SIN[ra];
    gi(0x00);
    gw(c); gw(s); gw(0);
    gw(-s); gw(c); gw(0);
    gw(0); gw(0); gw(16384);
    gw(-(fmul(c, camx, 14) + fmul(s, camy, 14)));
    gw(fmul(s, camx, 14) - fmul(c, camy, 14));
    gw(0);
    gi(0x56); gd(0); gd(0);                             /* FFIRST */
    gi(0x59); gd(NF); gi(0x6d); gd(NF >> 8);            /* NFACE */
    pass(0, 1); pass(NF, 1); pass(NF * 2, 1); pass(NF * 3, 7);
}

void main(void) {
    di();
    out(0xe4, 6); out(0xe5, 0x40);      /* turboR なら R800 にする（ほかの機械では何もしない） */
    screen();
    textures();
    world();
    geo_init();

    u8 back = 1;
    for (;;) {
        update();
        frame(back);
        while (st(2) & 0x40) {}
        while (!(st(2) & 0x40)) {}
        vr(2, (back << 5) | 0x1f);      /* 描いたページを見せる */
        back ^= 1;
        t++;
    }
}
