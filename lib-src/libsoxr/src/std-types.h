/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_std_types_included
#define soxr_std_types_included

#include "soxr-config.h"

#include <limits.h>

#if HAVE_STDBOOL_H
  #include <stdbool.h>
#else
  #undef bool
  #undef false
  #undef true
  #define bool int
  #define false 0
  #define true 1
#endif

#if HAVE_STDINT_H
  #include <stdint.h>
#else
  #undef int16_t
  #undef int32_t
  #undef int64_t
  #undef uint32_t
  #undef uint64_t
  #define int16_t short
  #if LONG_MAX > 2147483647L
    #define int32_t int
    #define int64_t long
  #elif LONG_MAX < 2147483647L
  #error this library requires that 'long int' has at least 32-bits
  #else
    #define int32_t long
    #if defined _MSC_VER
      #define int64_t __int64
    #else
      #define int64_t long long
    #endif
  #endif
  #define uint32_t unsigned int32_t
  #define uint64_t unsigned int64_t
#endif

#endif
