/* Some misc util functions for audio DSP work, written by Steve Harris,
 * December 2000
 *
 *  steve@plugin.org.uk
 */

#ifndef LADSPA_UTIL_H
#define LADSPA_UTIL_H

#include <math.h>
#include <stdint.h>

#include "config.h"

// 16.16 fixpoint
typedef union {
	int32_t all;
	struct {
#ifdef WORDS_BIGENDIAN
		int16_t in;
		uint16_t fr;
#else
		uint16_t fr;
		int16_t in;
#endif
	} part;
} fixp16;

// 32.32 fixpoint
typedef union {
	int64_t all;
	struct {
#ifdef WORDS_BIGENDIAN
		int32_t in;
		uint32_t fr;
#else
		uint32_t fr;
		int32_t in;
#endif
	} part;
} fixp32;

/* 32 bit "pointer cast" union */
typedef union {
        float f;
        int32_t i;
} ls_pcast32;

// Sometimes it doesn't get defined, even though it eists and C99 is declared
long int lrintf (float x);

// 1.0 / ln(2)
#define LN2R 1.442695041f

/* detet floating point denormal numbers by comparing them to the smallest
 * normal, crap, but reliable */
#define DN_CHECK(x, l) if (fabs(x) < 1e-38) printf("DN: "l"\n")

// Denormalise floats, only actually needed for PIII and recent PowerPC
//#define FLUSH_TO_ZERO(fv) (((*(unsigned int*)&(fv))&0x7f800000)==0)?0.0f:(fv)

static inline float flush_to_zero(float f)
{
	ls_pcast32 v;

	v.f = f;

	// original: return (v.i & 0x7f800000) == 0 ? 0.0f : f;
	// version from Tim Blechmann
	return (v.i & 0x7f800000) < 0x08000000 ? 0.0f : f;
}

static inline void round_to_zero(volatile float *f)
{
	*f += 1e-18;
	*f -= 1e-18;
}

/* A set of branchless clipping operations from Laurent de Soras */

static inline float f_max(float x, float a)
{
	x -= a;
	x += fabs(x);
	x *= 0.5;
	x += a;

	return x;
}

static inline float f_min(float x, float b)
{
	x = b - x;
	x += fabs(x);
	x *= 0.5;
	x = b - x;

	return x;
}

static inline float f_clamp(float x, float a, float b)
{
	const float x1 = fabs(x - a);
	const float x2 = fabs(x - b);

	x = x1 + a + b;
	x -= x2;
	x *= 0.5;

	return x;
}

// Limit a value to be l<=v<=u
#define LIMIT(v,l,u) ((v)<(l)?(l):((v)>(u)?(u):(v)))

// Truncate-to-zero modulo (ANSI C doesn't specify) will only work
// if -m < v < 2m
#define MOD(v,m) (v<0?v+m:(v>=m?v-m:v))

// Truncate-to-zero modulo (ANSI C doesn't specify) will only work
// if v > -m and v < m
#define NEG_MOD(v,m) ((v)<0?((v)+(m)):(v))

// Convert a value in dB's to a coefficent
#define DB_CO(g) ((g) > -90.0f ? powf(10.0f, (g) * 0.05f) : 0.0f)
#define CO_DB(v) (20.0f * log10f(v))

// Linearly interpolate [ = a * (1 - f) + b * f]
#define LIN_INTERP(f,a,b) ((a) + (f) * ((b) - (a)))

// Cubic interpolation function
static inline float cube_interp(const float fr, const float inm1, const float
                                in, const float inp1, const float inp2)
{
	return in + 0.5f * fr * (inp1 - inm1 +
	 fr * (4.0f * inp1 + 2.0f * inm1 - 5.0f * in - inp2 +
	 fr * (3.0f * (in - inp1) - inm1 + inp2)));
}

/* fast sin^2 aproxiamtion, adapted from jan AT rpgfan's posting to the
 * music-dsp list */
static inline float f_sin_sq(float angle)
{
	const float asqr = angle * angle;
	float result = -2.39e-08f;

	result *= asqr;
	result += 2.7526e-06f;
	result *= asqr;
	result -= 1.98409e-04f;
	result *= asqr;
	result += 8.3333315e-03f;
	result *= asqr;
	result -= 1.666666664e-01f;
	result *= asqr;
	result += 1.0f;
	result *= angle;

	return result * result;
}

#ifdef HAVE_LRINTF

#define f_round(f) lrintf(f)

#else

// Round float to int using IEEE int* hack
static inline int f_round(float f)
{
	ls_pcast32 p;

	p.f = f;
	p.f += (3<<22);

	return p.i - 0x4b400000;
}

#endif

// Truncate float to int
static inline int f_trunc(float f)
{
	return f_round(floorf(f));
}

/* Andrew Simper's pow(2, x) aproximation from the music-dsp list */

#if 0

/* original */
static inline float f_pow2(float x)
{
	long *px = (long*)(&x); // store address of float as long pointer
	const float tx = (x-0.5f) + (3<<22); // temporary value for truncation
	const long  lx = *((long*)&tx) - 0x4b400000; // integer power of 2
	const float dx = x-(float)(lx); // float remainder of power of 2

	x = 1.0f + dx*(0.6960656421638072f + // cubic apporoximation of 2^x
                   dx*(0.224494337302845f +  // for x in the range [0, 1]
                   dx*(0.07944023841053369f)));
	*px += (lx<<23); // add integer power of 2 to exponent

	return x;
}

#else

/* union version */
static inline float f_pow2(float x)
{
	ls_pcast32 *px, tx, lx;
	float dx;

	px = (ls_pcast32 *)&x; // store address of float as long pointer
	tx.f = (x-0.5f) + (3<<22); // temporary value for truncation
	lx.i = tx.i - 0x4b400000; // integer power of 2
	dx = x - (float)lx.i; // float remainder of power of 2

	x = 1.0f + dx * (0.6960656421638072f + // cubic apporoximation of 2^x
		   dx * (0.224494337302845f +  // for x in the range [0, 1]
		   dx * (0.07944023841053369f)));
	(*px).i += (lx.i << 23); // add integer power of 2 to exponent

	return (*px).f;
}

#endif

/* Fast exponentiation function, y = e^x */
#define f_exp(x) f_pow2(x * LN2R)

#endif
