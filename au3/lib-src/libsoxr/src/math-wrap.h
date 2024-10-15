/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_math_wrap_included
#define soxr_math_wrap_included

#include <math.h>

#if defined __STRICT_ANSI__
  #define sinf(x)  (float)sin ((double)(x))
  #define cosf(x)  (float)cos ((double)(x))
  #define atanf(x) (float)atan((double)(x))
#endif

#if !defined M_PI
  #define M_PI    3.141592653589793238462643383279502884
#endif

#if !defined M_LN10
  #define M_LN10  2.302585092994045684017991454684364208
#endif

#if !defined M_SQRT2
  #define M_SQRT2 1.414213562373095048801688724209698079
#endif

#if !defined M_LN2
  #define M_LN2   0.693147180559945309417232121458176568
#endif

#endif
