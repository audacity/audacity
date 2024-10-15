/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_rint_included
#define soxr_rint_included

#include "std-types.h"

/* For x86, compiler-supplied versions of these functions (where available)
 * can have poor performance (e.g. mingw32), so prefer these asm versions: */

#if defined __GNUC__ && (defined __i386__ || defined __x86_64__)
  #define FPU_RINT32
  #define FPU_RINT16
  #define rint32D(a,b) __asm__ __volatile__("fistpl %0": "=m"(a): "t"(b): "st")
  #define rint16D(a,b) __asm__ __volatile__("fistps %0": "=m"(a): "t"(b): "st")
  #define rint32F rint32D
  #define rint16F rint16D
  #define FE_INVALID 1
  static __inline int fe_test_invalid(void) {
    int status_word;
    __asm__ __volatile__("fnstsw %%ax": "=a"(status_word));
    return status_word & FE_INVALID;
  }
  static __inline int fe_clear_invalid(void) {
    int32_t status[7];
    __asm__ __volatile__("fnstenv %0": "=m"(status));
    status[1] &= ~FE_INVALID;
    __asm__ __volatile__("fldenv %0": : "m"(*status));
    return 0;
  }
#elif defined _MSC_VER && defined _M_IX86
  #define FPU_RINT32
  #define FPU_RINT16
  #define rint_fn(N,Y,X) \
    static __inline void N(Y *y, X x) {Y t; {__asm fld x __asm fistp t} *y=t;}
  rint_fn(rint32d, int32_t, double)
  rint_fn(rint32f, int32_t, float )
  rint_fn(rint16d, int16_t, double)
  rint_fn(rint16f, int16_t, float )
  #define rint32D(y,x) rint32d(&(y),x)
  #define rint32F(y,x) rint32f(&(y),x)
  #define rint16D(y,x) rint16d(&(y),x)
  #define rint16F(y,x) rint16f(&(y),x)
  #define FE_INVALID 1
  static __inline int fe_test_invalid(void) {
    short status_word;
    __asm fnstsw status_word
    return status_word & FE_INVALID;
  }
  static __inline int fe_clear_invalid(void) {
    int32_t status[7];
    __asm fnstenv status
    status[1] &= ~FE_INVALID;
    __asm fldenv status
    return 0;
  }
#elif defined _MSC_VER && defined _M_X64
  #include <emmintrin.h>
  #include <float.h>
  #define FPU_RINT32
  #define FPU_RINT16
  static __inline void rint32d(int32_t *y, double x) {
    *y = _mm_cvtsd_si32(_mm_load_sd(&x));}
  static __inline void rint32f(int32_t *y, float  x) {
    *y = _mm_cvtss_si32(_mm_load_ss(&x));}
  static __inline void rint16d(int16_t *y, double x) {
    x = x*65536+32738; *y = (int16_t)(_mm_cvtsd_si32(_mm_load_sd(&x)) >> 16);}
  #define rint32D(y,x) rint32d(&(y),x)
  #define rint32F(y,x) rint32f(&(y),x)
  #define rint16D(y,x) rint16d(&(y),x)
  #define rint16F(y,x) rint16d(&(y),(double)(x))
  #define FE_INVALID 1
  #define fe_test_invalid() (_statusfp() & _SW_INVALID)
  #define fe_clear_invalid _clearfp /* Note: clears all. */
#elif HAVE_LRINT && LONG_MAX == 2147483647L && HAVE_FENV_H
  #include <math.h>
  #include <fenv.h>
  #define FPU_RINT32
  #define rint32D(y,x) ((y)=lrint(x))
  #define rint32F(y,x) ((y)=lrintf(x))
  #define fe_test_invalid() fetestexcept(FE_INVALID)
  #define fe_clear_invalid() feclearexcept(FE_INVALID)
#endif

#if !defined FPU_RINT32
  #define rint32D(y,x) ((y)=(int32_t)((x) < 0? x - .5 : x + .5))
  #define rint32F(y,x) rint32D(y,(double)(x))
#endif

#if !defined FPU_RINT16
  #define rint16D(y,x) ((y)=(int16_t)((x) < 0? x - .5 : x + .5))
  #define rint16F(y,x) rint16D(y,(double)(x))
#endif

static __inline int32_t rint32(double input) {
  int32_t result; rint32D(result, input); return result;}

static __inline int16_t rint16(double input) {
  int16_t result; rint16D(result, input); return result;}

#endif
