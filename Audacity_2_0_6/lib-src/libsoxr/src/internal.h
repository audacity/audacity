/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_internal_included
#define soxr_internal_included

#include "soxr-config.h"

#undef min
#undef max
#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#define range_limit(x, lower, upper) (min(max(x, lower), upper))
#define linear_to_dB(x) (log10(x) * 20)
#define array_length(a) (sizeof(a)/sizeof(a[0]))
#define AL(a) array_length(a)
#define iAL(a) (int)AL(a)
#define sqr(a) ((a) * (a))

#ifdef __GNUC__
  #define UNUSED __attribute__ ((unused))
#else
  #define UNUSED
#endif

#if defined NDEBUG
  #ifdef __GNUC__
    void lsx_dummy(char const *, ...);
  #else
    static __inline void lsx_dummy(char const * x, ...) {}
  #endif
  #define lsx_debug if(0) lsx_dummy
#else
  #include <stdarg.h>
  #include <stdio.h>
  UNUSED static void lsx_debug(char const * fmt, ...)
  {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
    va_end(args);
  }
#endif
#endif
