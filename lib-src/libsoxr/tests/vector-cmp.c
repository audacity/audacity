/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Utility used to help test the library; not for general consumption.
 *
 * Measure the peak bit difference between two files.  */

#include <stdlib.h>
#include <stdio.h>
#include "../src/rint.h"
#include "../examples/examples-common.h"

#define TYPE 0 /* As vector-gen */

#if TYPE
  #define sample_t double
  #define N 50
  #define DIFF(s1,s2) abs(rint32((s1-s2)*ldexp(1,N-1)))
#else
  #define sample_t int32_t
  #define N 32
  #define DIFF(s1,s2) abs((int)(s1-s2))
#endif

int main(int argc, char const * arg[])
{
  int     two      = !!arg[2][0];
  FILE    * f1 = fopen(arg[1], "rb"), * f2 = two? fopen(arg[2], "rb") : 0;
  double  rate     = atof (arg[3]), /* Sample-rate */
          skip_len = atof (arg[4]), /* Skip length in seconds */
          len      = atof (arg[5]), /* Compare length in seconds */ r;
  int i = 0, count = rint32(rate * len), max = 0, diff;
  sample_t s1, s2;

  fseek(f1, rint32(rate * skip_len) * (int)sizeof(s1), SEEK_CUR);
  if (two) {
    fseek(f2, rint32(rate * skip_len) * (int)sizeof(s2), SEEK_CUR);
    for (; i < count &&
        fread(&s1, sizeof(s1), 1, f1) &&
        fread(&s2, sizeof(s2), 1, f2); ++i) {
      diff = DIFF(s1, s2);
      max = max(max, diff);
    }
  }
  else for (; i < count && fread(&s1, sizeof(s1), 1, f1); ++i) {
    diff = DIFF(s1, 0);
    max = max(max, diff);
  }

  if (i != count) {
    fprintf(stderr, "incorrect file length\n");
    return 1;
  }
  printf("%f\n", r = N-log(max)/log(2));
  return argc>6? r<atof(arg[6]) : 0;
}
