/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_rint_included
#define soxr_rint_included

#include "soxr-config.h"



#if HAVE_LRINT && LONG_MAX == 2147483647L
  #include <math.h>
  #define FPU_RINT32
  #define rint32 lrint
#elif defined __GNUC__ && (defined __i386__ || defined __x86_64__)
  #define FPU_RINT32
  static __inline int32_t rint32(double input) {
    int32_t result;
    __asm__ __volatile__("fistpl %0": "=m"(result): "t"(input): "st");
    return result;
  }
#elif defined __GNUC__ && defined __arm__
  #define FPU_RINT32
  static __inline int32_t rint32(double input) {
    register int32_t result;
    __asm__ __volatile__ ("ftosid %0, %P1": "=w"(result): "w"(input));
    return result;
  }
#elif defined _MSC_VER && defined _M_IX86 /* FIXME need solution for MSVC x64 */
  #define FPU_RINT32
  static __inline int32_t rint32(double input) {
    int32_t result;
    _asm {
      fld input
      fistp result
    }
    return result;
  }
#else
  #define rint32(x) (int32_t)((x) < 0? x - .5 : x + .5)
#endif



#if defined __GNUC__ && (defined __i386__ || defined __x86_64__)
  #define FPU_RINT16
  static __inline int16_t rint16(double input) {
    int16_t result;
    __asm__ __volatile__("fistps %0": "=m"(result): "t"(input): "st");
    return result;
  }
#elif defined _MSC_VER && defined _M_IX86 /* FIXME need solution for MSVC x64 */
  #define FPU_RINT16
  static __inline int16_t rint16(double input) {
    int16_t result;
    _asm {
      fld input
      fistp result
    }
    return result;
  }
#else
  #define rint16(x) (int16_t)((x) < 0? x - .5 : x + .5)
#endif



#endif
