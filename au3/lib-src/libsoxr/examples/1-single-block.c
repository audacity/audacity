/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 1: `One-shot' resample a single block of data in memory.
 *
 * N.B. See example 2 for how to resample a stream (of blocks).
 *
 * Optional arguments are: INPUT-RATE OUTPUT-RATE
 *
 * With the default arguments, the output should produce lines similar to the
 * following:
 *
 *  0.00  0.71  1.00  0.71 -0.00 -0.71 -1.00 -0.71
 *
 * Gibbs effect may be seen at the ends of the resampled signal; this is because
 * unlike a `real-world' signal, the synthetic input signal is not band-limited.
 */

#include <soxr.h>
#include "examples-common.h"

const float in[] = {  /* Input: 12 cycles of a sine wave with freq. = irate/4 */
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1,
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1};

int main(int argc, char const * arg[])
{
  double irate = argc > 1? atof(arg[1]) : 1;      /* Default to interpolation */
  double orate = argc > 2? atof(arg[2]) : 2;             /* by a factor of 2. */

  size_t olen = (size_t)(AL(in) * orate / irate + .5);   /* Assay output len. */
  float * out = malloc(sizeof(*out) * olen);       /* Allocate output buffer. */
  size_t odone;

  soxr_error_t error = soxr_oneshot(irate, orate, 1, /* Rates and # of chans. */
      in, AL(in), NULL,                              /* Input. */
      out, olen, &odone,                             /* Output. */
      NULL, NULL, NULL);                             /* Default configuration.*/

  unsigned i = 0;                            /* Print out the resampled data, */
  while (i++ < odone)
    printf("%5.2f%c", out[i-1], " \n"[!(i&7) || i == odone]);
  printf("%-26s %s\n", arg[0], soxr_strerror(error)); /* and reported result. */

  if (argc > 3)                                     /* Library version check: */
    printf("runtime=%s API="SOXR_THIS_VERSION_STR"\n", soxr_version());

  free(out);                                                      /* Tidy up. */
  return !!error;
}
