/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* AVX support macros */

#if !defined soxr_avx_included
#define soxr_avx_included

#include <immintrin.h>

typedef __m256d v4sf;
#define VZERO() _mm256_setzero_pd()
#define VMUL(a,b) _mm256_mul_pd(a,b)
#define VADD(a,b) _mm256_add_pd(a,b)
#define VMADD(a,b,c) VADD(VMUL(a,b),c) /* Note: gcc -mfma will `fuse' these */
#define VSUB(a,b) _mm256_sub_pd(a,b)
#define LD_PS1(p) _mm256_set1_pd(p)
#define INTERLEAVE2(in1, in2, out1, out2) {v4sf \
  t1 = _mm256_unpacklo_pd(in1, in2), \
  t2 = _mm256_unpackhi_pd(in1, in2); \
  out1 = _mm256_permute2f128_pd(t1,t2,0x20); \
  out2 = _mm256_permute2f128_pd(t1,t2,0x31); }
#define UNINTERLEAVE2(in1, in2, out1, out2) {v4sf \
  t1 = _mm256_permute2f128_pd(in1,in2,0x20), \
  t2 = _mm256_permute2f128_pd(in1,in2,0x31); \
  out1 = _mm256_unpacklo_pd(t1, t2); \
  out2 = _mm256_unpackhi_pd(t1, t2);}
#define VTRANSPOSE4(x0,x1,x2,x3) {v4sf \
  t0 = _mm256_shuffle_pd(x0,x1, 0x0), \
  t2 = _mm256_shuffle_pd(x0,x1, 0xf), \
  t1 = _mm256_shuffle_pd(x2,x3, 0x0), \
  t3 = _mm256_shuffle_pd(x2,x3, 0xf); \
  x0 = _mm256_permute2f128_pd(t0,t1, 0x20); \
  x1 = _mm256_permute2f128_pd(t2,t3, 0x20); \
  x2 = _mm256_permute2f128_pd(t0,t1, 0x31); \
  x3 = _mm256_permute2f128_pd(t2,t3, 0x31);}
#define VSWAPHL(a,b) _mm256_permute2f128_pd(b, a, 0x30)
#define VALIGNED(ptr) ((((long)(ptr)) & 0x1F) == 0)

#endif
