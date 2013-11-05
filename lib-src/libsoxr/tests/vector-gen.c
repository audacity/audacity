/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Utility used to help test the library; not for general consumption.
 *
 * Generate a swept sine to a file, with faded `lead-in' section.  */

#define QUAD 0

#if QUAD
  #include <quadmath.h>
#endif

#include "../examples/examples-common.h"

#if QUAD
  #define modf modfq
  #define cos cosq
  #define sin sinq
  #undef M_PI
  #define M_PI M_PIq
  #define real __float128
  #define atof(x) strtoflt128(x, 0)
#else
  #define real double
  #include "rint.h"
#endif

int main(int i, char const * argv[])
{
  real rate           = atof(argv[1]), /* Rate for this vector */
       lead_in_len    = atof(argv[2]), /* Lead-in length in seconds */
       len            = atof(argv[3]), /* Sweep length (excl. lead_in_len) */
       sweep_to_freq  = atof(argv[4]), /* Sweep from DC to this freq. */
       multiplier     = atof(argv[5]), /* For headroom */
       f1 = -sweep_to_freq / len * lead_in_len, f2 = sweep_to_freq,
       n1 = rate * -lead_in_len, n2 = rate * len,
       m = (f2 - f1) / (n2 - n1) / 2, dummy;
  FILE * file = fopen(argv[6], "wb");
  i = (int)n1;
  if (!file || i != n1)
    exit(1);
  for (; i < (int)(n2 + .5); ++i) {
    double d1 = multiplier * sin(2 * M_PI * modf(i * m * i / rate, &dummy));
    double d = i < 0? d1 * (1 - cos(M_PI * (i + n1) / n1)) * .5 : d1;
#if QUAD
    size_t actual = fwrite(&d, sizeof(d), 1, file);
#else
    int32_t out = rint32(d * (32768. * 65536 - 1));
    size_t actual = fwrite(&out, sizeof(out), 1, file);
#endif
    if (actual != 1)
      return 1;
  }
  return 0;
}
