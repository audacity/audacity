/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_dev64s_included
#define soxr_dev64s_included

#if defined __GNUC__
  #define SIMD_INLINE(T) static __inline T __attribute__((always_inline))
  #define vAlign __attribute__((aligned (32)))
#elif defined _MSC_VER
  #define SIMD_INLINE(T) static __forceinline T
  #define vAlign __declspec(align(32))
#else
  #define SIMD_INLINE(T) static __inline T
#endif

#if defined __x86_64__ || defined _M_X64 || defined i386 || defined _M_IX86

#include <immintrin.h>

#if defined __AVX__

#define vZero()      _mm256_setzero_pd()
#define vSet1(a)     _mm256_set_pd(0,0,0,a)
#define vMul(a,b)    _mm256_mul_pd(a,b)
#define vAdd(a,b)    _mm256_add_pd(a,b)
#define vMac(a,b,c)  vAdd(vMul(a,b),c) /* Note: gcc -mfma will `fuse' these */
#define vLds(a)      _mm256_set1_pd(a)
#define vLd(a)       _mm256_load_pd(a)
#define vLdu(a)      _mm256_loadu_pd(a)

typedef __m256d v4_t;

SIMD_INLINE(void) vStorSum(double * a, v4_t b) {
  b = _mm256_hadd_pd(b, _mm256_permute2f128_pd(b,b,1));
  _mm_store_sd(a, _mm256_castpd256_pd128(_mm256_hadd_pd(b,b)));}

#endif

#endif

#endif
