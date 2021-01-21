/*
 *  TCC runtime library for arm64.
 *
 *  Copyright (c) 2015 Edmund Grimley Evans
 *
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */

#ifdef __TINYC__
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;
void *memcpy(void*,void*,__SIZE_TYPE__);
#else
#include <stdint.h>
#include <string.h>
#endif

void __clear_cache(void *beg, void *end)
{
    __arm64_clear_cache(beg, end);
}

typedef struct {
    uint64_t x0, x1;
} u128_t;

static long double f3_zero(int sgn)
{
    long double f;
    u128_t x = { 0, (uint64_t)sgn << 63 };
    memcpy(&f, &x, 16);
    return f;
}

static long double f3_infinity(int sgn)
{
    long double f;
    u128_t x = { 0, (uint64_t)sgn << 63 | 0x7fff000000000000 };
    memcpy(&f, &x, 16);
    return f;
}

static long double f3_NaN(void)
{
    long double f;
#if 0
    // ARM's default NaN usually has just the top fraction bit set:
    u128_t x = {  0, 0x7fff800000000000 };
#else
    // GCC's library sets all fraction bits:
    u128_t x = { -1, 0x7fffffffffffffff };
#endif
    memcpy(&f, &x, 16);
    return f;
}

static int fp3_convert_NaN(long double *f, int sgn, u128_t mnt)
{
    u128_t x = { mnt.x0,
                 mnt.x1 | 0x7fff800000000000 | (uint64_t)sgn << 63 };
    memcpy(f, &x, 16);
    return 1;
}

static int fp3_detect_NaNs(long double *f,
                           int a_sgn, int a_exp, u128_t a,
                           int b_sgn, int b_exp, u128_t b)
{
    // Detect signalling NaNs:
    if (a_exp == 32767 && (a.x0 | a.x1 << 16) && !(a.x1 >> 47 & 1))
        return fp3_convert_NaN(f, a_sgn, a);
    if (b_exp == 32767 && (b.x0 | b.x1 << 16) && !(b.x1 >> 47 & 1))
        return fp3_convert_NaN(f, b_sgn, b);

    // Detect quiet NaNs:
    if (a_exp == 32767 && (a.x0 | a.x1 << 16))
        return fp3_convert_NaN(f, a_sgn, a);
    if (b_exp == 32767 && (b.x0 | b.x1 << 16))
        return fp3_convert_NaN(f, b_sgn, b);

    return 0;
}

static void f3_unpack(int *sgn, int32_t *exp, u128_t *mnt, long double f)
{
    u128_t x;
    memcpy(&x, &f, 16);
    *sgn = x.x1 >> 63;
    *exp = x.x1 >> 48 & 32767;
    x.x1 = x.x1 << 16 >> 16;
    if (*exp)
        x.x1 |= (uint64_t)1 << 48;
    else
        *exp = 1;
    *mnt = x;
}

static u128_t f3_normalise(int32_t *exp, u128_t mnt)
{
    int sh;
    if (!(mnt.x0 | mnt.x1))
        return mnt;
    if (!mnt.x1) {
        mnt.x1 = mnt.x0;
        mnt.x0 = 0;
        *exp -= 64;
    }
    for (sh = 32; sh; sh >>= 1) {
        if (!(mnt.x1 >> (64 - sh))) {
            mnt.x1 = mnt.x1 << sh | mnt.x0 >> (64 - sh);
            mnt.x0 = mnt.x0 << sh;
            *exp -= sh;
        }
    }
    return mnt;
}

static u128_t f3_sticky_shift(int32_t sh, u128_t x)
{
  if (sh >= 128) {
      x.x0 = !!(x.x0 | x.x1);
      x.x1 = 0;
      return x;
  }
  if (sh >= 64) {
      x.x0 = x.x1 | !!x.x0;
      x.x1 = 0;
      sh -= 64;
  }
  if (sh > 0) {
      x.x0 = x.x0 >> sh | x.x1 << (64 - sh) | !!(x.x0 << (64 - sh));
      x.x1 = x.x1 >> sh;
  }
  return x;
}

static long double f3_round(int sgn, int32_t exp, u128_t x)
{
    long double f;
    int error;

    if (exp > 0) {
        x = f3_sticky_shift(13, x);
    }
    else {
        x = f3_sticky_shift(14 - exp, x);
        exp = 0;
    }

    error = x.x0 & 3;
    x.x0 = x.x0 >> 2 | x.x1 << 62;
    x.x1 = x.x1 >> 2;

    if (error == 3 || ((error == 2) & (x.x0 & 1))) {
        if (!++x.x0) {
            ++x.x1;
            if (x.x1 == (uint64_t)1 << 48)
                exp = 1;
            else if (x.x1 == (uint64_t)1 << 49) {
                ++exp;
                x.x0 = x.x0 >> 1 | x.x1 << 63;
                x.x1 = x.x1 >> 1;
            }
        }
    }

    if (exp >= 32767)
        return f3_infinity(sgn);

    x.x1 = x.x1 << 16 >> 16 | (uint64_t)exp << 48 | (uint64_t)sgn << 63;
    memcpy(&f, &x, 16);
    return f;
}

static long double f3_add(long double fa, long double fb, int neg)
{
    u128_t a, b, x;
    int32_t a_exp, b_exp, x_exp;
    int a_sgn, b_sgn, x_sgn;
    long double fx;

    f3_unpack(&a_sgn, &a_exp, &a, fa);
    f3_unpack(&b_sgn, &b_exp, &b, fb);

    if (fp3_detect_NaNs(&fx, a_sgn, a_exp, a, b_sgn, b_exp, b))
        return fx;

    b_sgn ^= neg;

    // Handle infinities and zeroes:
    if (a_exp == 32767 && b_exp == 32767 && a_sgn != b_sgn)
        return f3_NaN();
    if (a_exp == 32767)
        return f3_infinity(a_sgn);
    if (b_exp == 32767)
        return f3_infinity(b_sgn);
    if (!(a.x0 | a.x1 | b.x0 | b.x1))
        return f3_zero(a_sgn & b_sgn);

    a.x1 = a.x1 << 3 | a.x0 >> 61;
    a.x0 = a.x0 << 3;
    b.x1 = b.x1 << 3 | b.x0 >> 61;
    b.x0 = b.x0 << 3;

    if (a_exp <= b_exp) {
        a = f3_sticky_shift(b_exp - a_exp, a);
        a_exp = b_exp;
    }
    else {
        b = f3_sticky_shift(a_exp - b_exp, b);
        b_exp = a_exp;
    }

    x_sgn = a_sgn;
    x_exp = a_exp;
    if (a_sgn == b_sgn) {
        x.x0 = a.x0 + b.x0;
        x.x1 = a.x1 + b.x1 + (x.x0 < a.x0);
    }
    else {
        x.x0 = a.x0 - b.x0;
        x.x1 = a.x1 - b.x1 - (x.x0 > a.x0);
        if (x.x1 >> 63) {
            x_sgn ^= 1;
            x.x0 = -x.x0;
            x.x1 = -x.x1 - !!x.x0;
        }
    }

    if (!(x.x0 | x.x1))
        return f3_zero(0);

    x = f3_normalise(&x_exp, x);

    return f3_round(x_sgn, x_exp + 12, x);
}

long double __addtf3(long double a, long double b)
{
    return f3_add(a, b, 0);
}

long double __subtf3(long double a, long double b)
{
    return f3_add(a, b, 1);
}

long double __multf3(long double fa, long double fb)
{
    u128_t a, b, x;
    int32_t a_exp, b_exp, x_exp;
    int a_sgn, b_sgn, x_sgn;
    long double fx;

    f3_unpack(&a_sgn, &a_exp, &a, fa);
    f3_unpack(&b_sgn, &b_exp, &b, fb);

    if (fp3_detect_NaNs(&fx, a_sgn, a_exp, a, b_sgn, b_exp, b))
        return fx;

    // Handle infinities and zeroes:
    if ((a_exp == 32767 && !(b.x0 | b.x1)) ||
        (b_exp == 32767 && !(a.x0 | a.x1)))
        return f3_NaN();
    if (a_exp == 32767 || b_exp == 32767)
        return f3_infinity(a_sgn ^ b_sgn);
    if (!(a.x0 | a.x1) || !(b.x0 | b.x1))
        return f3_zero(a_sgn ^ b_sgn);

    a = f3_normalise(&a_exp, a);
    b = f3_normalise(&b_exp, b);

    x_sgn = a_sgn ^ b_sgn;
    x_exp = a_exp + b_exp - 16352;

    {
        // Convert to base (1 << 30), discarding bottom 6 bits, which are zero,
        // so there are (32, 30, 30, 30) bits in (a3, a2, a1, a0):
        uint64_t a0 = a.x0 << 28 >> 34;
        uint64_t b0 = b.x0 << 28 >> 34;
        uint64_t a1 = a.x0 >> 36 | a.x1 << 62 >> 34;
        uint64_t b1 = b.x0 >> 36 | b.x1 << 62 >> 34;
        uint64_t a2 = a.x1 << 32 >> 34;
        uint64_t b2 = b.x1 << 32 >> 34;
        uint64_t a3 = a.x1 >> 32;
        uint64_t b3 = b.x1 >> 32;
        // Use 16 small multiplications and additions that do not overflow:
        uint64_t x0 = a0 * b0;
        uint64_t x1 = (x0 >> 30) + a0 * b1 + a1 * b0;
        uint64_t x2 = (x1 >> 30) + a0 * b2 + a1 * b1 + a2 * b0;
        uint64_t x3 = (x2 >> 30) + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0;
        uint64_t x4 = (x3 >> 30) + a1 * b3 + a2 * b2 + a3 * b1;
        uint64_t x5 = (x4 >> 30) + a2 * b3 + a3 * b2;
        uint64_t x6 = (x5 >> 30) + a3 * b3;
        // We now have (64, 30, 30, ...) bits in (x6, x5, x4, ...).
        // Take the top 128 bits, setting bottom bit if any lower bits were set:
        uint64_t y0 = (x5 << 34 | x4 << 34 >> 30 | x3 << 34 >> 60 |
                       !!(x3 << 38 | (x2 | x1 | x0) << 34));
        uint64_t y1 = x6;
        // Top bit may be zero. Renormalise:
        if (!(y1 >> 63)) {
            y1 = y1 << 1 | y0 >> 63;
            y0 = y0 << 1;
            --x_exp;
        }
        x.x0 = y0;
        x.x1 = y1;
    }

    return f3_round(x_sgn, x_exp, x);
}

long double __divtf3(long double fa, long double fb)
{
    u128_t a, b, x;
    int32_t a_exp, b_exp, x_exp;
    int a_sgn, b_sgn, x_sgn, i;
    long double fx;

    f3_unpack(&a_sgn, &a_exp, &a, fa);
    f3_unpack(&b_sgn, &b_exp, &b, fb);

    if (fp3_detect_NaNs(&fx, a_sgn, a_exp, a, b_sgn, b_exp, b))
        return fx;

    // Handle infinities and zeroes:
    if ((a_exp == 32767 && b_exp == 32767) ||
        (!(a.x0 | a.x1) && !(b.x0 | b.x1)))
        return f3_NaN();
    if (a_exp == 32767 || !(b.x0 | b.x1))
        return f3_infinity(a_sgn ^ b_sgn);
    if (!(a.x0 | a.x1) || b_exp == 32767)
        return f3_zero(a_sgn ^ b_sgn);

    a = f3_normalise(&a_exp, a);
    b = f3_normalise(&b_exp, b);

    x_sgn = a_sgn ^ b_sgn;
    x_exp = a_exp - b_exp + 16395;

    a.x0 = a.x0 >> 1 | a.x1 << 63;
    a.x1 = a.x1 >> 1;
    b.x0 = b.x0 >> 1 | b.x1 << 63;
    b.x1 = b.x1 >> 1;
    x.x0 = 0;
    x.x1 = 0;
    for (i = 0; i < 116; i++) {
        x.x1 = x.x1 << 1 | x.x0 >> 63;
        x.x0 = x.x0 << 1;
        if (a.x1 > b.x1 || (a.x1 == b.x1 && a.x0 >= b.x0)) {
            a.x1 = a.x1 - b.x1 - (a.x0 < b.x0);
            a.x0 = a.x0 - b.x0;
            x.x0 |= 1;
        }
        a.x1 = a.x1 << 1 | a.x0 >> 63;
        a.x0 = a.x0 << 1;
    }
    x.x0 |= !!(a.x0 | a.x1);

    x = f3_normalise(&x_exp, x);

    return f3_round(x_sgn, x_exp, x);
}

long double __extendsftf2(float f)
{
    long double fx;
    u128_t x;
    uint32_t a;
    uint64_t aa;
    memcpy(&a, &f, 4);
    aa = a;
    x.x0 = 0;
    if (!(a << 1))
        x.x1 = aa << 32;
    else if (a << 1 >> 24 == 255)
        x.x1 = (0x7fff000000000000 | aa >> 31 << 63 | aa << 41 >> 16 |
                (uint64_t)!!(a << 9) << 47);
    else
        x.x1 = (aa >> 31 << 63 | ((aa >> 23 & 255) + 16256) << 48 |
                aa << 41 >> 16);
    memcpy(&fx, &x, 16);
    return fx;
}

long double __extenddftf2(double f)
{
    long double fx;
    u128_t x;
    uint64_t a;
    memcpy(&a, &f, 8);
    x.x0 = a << 60;
    if (!(a << 1))
        x.x1 = a;
    else if (a << 1 >> 53 == 2047)
        x.x1 = (0x7fff000000000000 | a >> 63 << 63 | a << 12 >> 16 |
                (uint64_t)!!(a << 12) << 47);
    else
        x.x1 = a >> 63 << 63 | ((a >> 52 & 2047) + 15360) << 48 | a << 12 >> 16;
    memcpy(&fx, &x, 16);
    return fx;
}

float __trunctfsf2(long double f)
{
    u128_t mnt;
    int32_t exp;
    int sgn;
    uint32_t x;
    float fx;

    f3_unpack(&sgn, &exp, &mnt, f);

    if (exp == 32767 && (mnt.x0 | mnt.x1 << 16))
        x = 0x7fc00000 | (uint32_t)sgn << 31 | (mnt.x1 >> 25 & 0x007fffff);
    else if (exp > 16510)
        x = 0x7f800000 | (uint32_t)sgn << 31;
    else if (exp < 16233)
        x = (uint32_t)sgn << 31;
    else {
        exp -= 16257;
        x = mnt.x1 >> 23 | !!(mnt.x0 | mnt.x1 << 41);
        if (exp < 0) {
            x = x >> -exp | !!(x << (32 + exp));
            exp = 0;
        }
        if ((x & 3) == 3 || (x & 7) == 6)
            x += 4;
        x = ((x >> 2) + (exp << 23)) | (uint32_t)sgn << 31;
    }
    memcpy(&fx, &x, 4);
    return fx;
}

double __trunctfdf2(long double f)
{
    u128_t mnt;
    int32_t exp;
    int sgn;
    uint64_t x;
    double fx;

    f3_unpack(&sgn, &exp, &mnt, f);

    if (exp == 32767 && (mnt.x0 | mnt.x1 << 16))
        x = (0x7ff8000000000000 | (uint64_t)sgn << 63 |
             mnt.x1 << 16 >> 12 | mnt.x0 >> 60);
    else if (exp > 17406)
        x = 0x7ff0000000000000 | (uint64_t)sgn << 63;
    else if (exp < 15308)
        x = (uint64_t)sgn << 63;
    else {
        exp -= 15361;
        x = mnt.x1 << 6 | mnt.x0 >> 58 | !!(mnt.x0 << 6);
        if (exp < 0) {
            x = x >> -exp | !!(x << (64 + exp));
            exp = 0;
        }
        if ((x & 3) == 3 || (x & 7) == 6)
            x += 4;
        x = ((x >> 2) + ((uint64_t)exp << 52)) | (uint64_t)sgn << 63;
    }
    memcpy(&fx, &x, 8);
    return fx;
}

int32_t __fixtfsi(long double fa)
{
    u128_t a;
    int32_t a_exp;
    int a_sgn;
    int32_t x;
    f3_unpack(&a_sgn, &a_exp, &a, fa);
    if (a_exp < 16369)
        return 0;
    if (a_exp > 16413)
        return a_sgn ? -0x80000000 : 0x7fffffff;
    x = a.x1 >> (16431 - a_exp);
    return a_sgn ? -x : x;
}

int64_t __fixtfdi(long double fa)
{
    u128_t a;
    int32_t a_exp;
    int a_sgn;
    int64_t x;
    f3_unpack(&a_sgn, &a_exp, &a, fa);
    if (a_exp < 16383)
        return 0;
    if (a_exp > 16445)
        return a_sgn ? -0x8000000000000000 : 0x7fffffffffffffff;
    x = (a.x1 << 15 | a.x0 >> 49) >> (16446 - a_exp);
    return a_sgn ? -x : x;
}

uint32_t __fixunstfsi(long double fa)
{
    u128_t a;
    int32_t a_exp;
    int a_sgn;
    f3_unpack(&a_sgn, &a_exp, &a, fa);
    if (a_sgn || a_exp < 16369)
        return 0;
    if (a_exp > 16414)
        return -1;
    return a.x1 >> (16431 - a_exp);
}

uint64_t __fixunstfdi(long double fa)
{
    u128_t a;
    int32_t a_exp;
    int a_sgn;
    f3_unpack(&a_sgn, &a_exp, &a, fa);
    if (a_sgn || a_exp < 16383)
        return 0;
    if (a_exp > 16446)
        return -1;
    return (a.x1 << 15 | a.x0 >> 49) >> (16446 - a_exp);
}

long double __floatsitf(int32_t a)
{
    int sgn = 0;
    int exp = 16414;
    uint32_t mnt = a;
    u128_t x = { 0, 0 };
    long double f;
    int i;
    if (a) {
        if (a < 0) {
            sgn = 1;
            mnt = -mnt;
        }
        for (i = 16; i; i >>= 1)
            if (!(mnt >> (32 - i))) {
                mnt <<= i;
                exp -= i;
            }
        x.x1 = ((uint64_t)sgn << 63 | (uint64_t)exp << 48 |
                (uint64_t)(mnt << 1) << 16);
    }
    memcpy(&f, &x, 16);
    return f;
}

long double __floatditf(int64_t a)
{
    int sgn = 0;
    int exp = 16446;
    uint64_t mnt = a;
    u128_t x = { 0, 0 };
    long double f;
    int i;
    if (a) {
        if (a < 0) {
            sgn = 1;
            mnt = -mnt;
        }
        for (i = 32; i; i >>= 1)
            if (!(mnt >> (64 - i))) {
                mnt <<= i;
                exp -= i;
            }
        x.x0 = mnt << 49;
        x.x1 = (uint64_t)sgn << 63 | (uint64_t)exp << 48 | mnt << 1 >> 16;
    }
    memcpy(&f, &x, 16);
    return f;
}

long double __floatunsitf(uint32_t a)
{
    int exp = 16414;
    uint32_t mnt = a;
    u128_t x = { 0, 0 };
    long double f;
    int i;
    if (a) {
        for (i = 16; i; i >>= 1)
            if (!(mnt >> (32 - i))) {
                mnt <<= i;
                exp -= i;
            }
        x.x1 = (uint64_t)exp << 48 | (uint64_t)(mnt << 1) << 16;
    }
    memcpy(&f, &x, 16);
    return f;
}

long double __floatunditf(uint64_t a)
{
    int exp = 16446;
    uint64_t mnt = a;
    u128_t x = { 0, 0 };
    long double f;
    int i;
    if (a) {
        for (i = 32; i; i >>= 1)
            if (!(mnt >> (64 - i))) {
                mnt <<= i;
                exp -= i;
            }
        x.x0 = mnt << 49;
        x.x1 = (uint64_t)exp << 48 | mnt << 1 >> 16;
    }
    memcpy(&f, &x, 16);
    return f;
}

static int f3_cmp(long double fa, long double fb)
{
    u128_t a, b;
    memcpy(&a, &fa, 16);
    memcpy(&b, &fb, 16);
    return (!(a.x0 | a.x1 << 1 | b.x0 | b.x1 << 1) ? 0 :
            ((a.x1 << 1 >> 49 == 0x7fff && (a.x0 | a.x1 << 16)) ||
             (b.x1 << 1 >> 49 == 0x7fff && (b.x0 | b.x1 << 16))) ? 2 :
            a.x1 >> 63 != b.x1 >> 63 ? (int)(b.x1 >> 63) - (int)(a.x1 >> 63) :
            a.x1 < b.x1 ? (int)(a.x1 >> 63 << 1) - 1 :
            a.x1 > b.x1 ? 1 - (int)(a.x1 >> 63 << 1) :
            a.x0 < b.x0 ? (int)(a.x1 >> 63 << 1) - 1 :
            b.x0 < a.x0 ? 1 - (int)(a.x1 >> 63 << 1) : 0);
}

int __eqtf2(long double a, long double b)
{
    return !!f3_cmp(a, b);
}

int __netf2(long double a, long double b)
{
    return !!f3_cmp(a, b);
}

int __lttf2(long double a, long double b)
{
    return f3_cmp(a, b);
}

int __letf2(long double a, long double b)
{
    return f3_cmp(a, b);
}

int __gttf2(long double a, long double b)
{
    return -f3_cmp(b, a);
}

int __getf2(long double a, long double b)
{
    return -f3_cmp(b, a);
}
