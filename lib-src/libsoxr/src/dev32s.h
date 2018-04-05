/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_dev32s_included
#define soxr_dev32s_included

#if defined __GNUC__
  #define SIMD_INLINE(T) static __inline T __attribute__((always_inline))
  #define vAlign __attribute__((aligned (16)))
#elif defined _MSC_VER
  #define SIMD_INLINE(T) static __forceinline T
  #define vAlign __declspec(align(16))
#endif

#if defined __x86_64__ || defined _M_X64 || defined i386 || defined _M_IX86

#include <xmmintrin.h>

#define vZero()      _mm_setzero_ps()
#define vSet1(a)     _mm_set_ss(a)
#define vMul(a,b)    _mm_mul_ps(a,b)
#define vAdd(a,b)    _mm_add_ps(a,b)
#define vMac(a,b,c)  vAdd(vMul(a,b),c)
#define vLds(a)      _mm_set1_ps(a)
#define vLd(a)       _mm_load_ps(a)
#define vLdu(a)      _mm_loadu_ps(a)

typedef __m128 v4_t;

SIMD_INLINE(void) vStorSum(float * a, v4_t b) {
  v4_t t = vAdd(_mm_movehl_ps(b, b), b);
  _mm_store_ss(a, vAdd(t, _mm_shuffle_ps(t,t,1)));}

#elif defined __arm__

#include <arm_neon.h>

#define vZero()      vdupq_n_f32(0)
#define vMul(a,b)    vmulq_f32(a,b)
#define vAdd(a,b)    vaddq_f32(a,b)
#define vMac(a,b,c)  vmlaq_f32(c,a,b)
#define vLds(a)      vld1q_dup_f32(&(a))
#define vLd(a)       vld1q_f32(a)
#define vLdu(a)      vld1q_f32(a)

typedef float32x4_t v4_t;

SIMD_INLINE(void) vStorSum(float * a, v4_t b) {
  float32x2_t t = vadd_f32(vget_high_f32(b), vget_low_f32(b));
  *a = vget_lane_f32(vpadd_f32(t, t), 0);}

#endif

#endif
