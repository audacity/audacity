/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Utility used to help test the library; not for general consumption.
 *
 * Compare two swept-sine files.  */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "../src/rint.h"

int main(int bit, char const * arg[])
{
  FILE    * f1       = fopen(arg[1], "rb"),
          * f2       = fopen(arg[2], "rb");
  double  rate       = atof (arg[3]), /* Rate for this vector */
          leader_len = atof (arg[4]), /* Leader length in seconds */
          len        = atof (arg[5]), /* Sweep length (excl. leader_len) */
          expect_bits= atof (arg[6]),
          expect_bw  = atof (arg[7]);

  int32_t s1, s2;
  long count = 0;
  static long thresh[32];
  double bw, prev = 0;

  for (; fread(&s1, sizeof(s1), 1, f1) == 1 &&
         fread(&s2, sizeof(s2), 1, f2) == 1; ++count) {
    long diff = abs((int)(s1 - s2));
    for (bit = 0; diff && bit < 32; bit++, diff >>= 1)
      if ((diff & 1) && !thresh[bit])
        thresh[bit] = count + 1;
  }

  if (count != (long)((leader_len + len) * rate + .5)) {
    printf("incorrect file length\n");
    exit(1);
  }

  for (bit = 0; bit < 32; ++bit) {
    bw = ((double)thresh[bit] - 1) / rate - leader_len;
    if (bit && bw >= 0 && (bw - prev) * 100 / len < .08) {
      --bit;
      break;
    }
    prev = bw;
  }
  bit = 32 - bit;
  bw = bw * 100 / len;
  printf("Bit perfect to %i bits, from DC to %.2f%% nyquist.\n", bit, bw);
  return !(bit >= expect_bits && bw >= expect_bw);
}
