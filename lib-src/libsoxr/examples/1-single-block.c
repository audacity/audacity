/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 1: `One-shot' resample a single block of data in memory.
 *
 * N.B. See example 2 for how to resample a stream (of blocks).
 *
 * Optional arguments are: INPUT-RATE OUTPUT-RATE
 */

#include "util.h"
#include <soxr.h>

const float in[] = {  /* Input: 12 cycles of a sine wave with freq. = irate/4 */
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1,
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1};

int main(int argc, char const * arg[])
{
  double irate = argc > 1? atof(arg[1]) : 1;         /* Default to upsampling */
  double orate = argc > 2? atof(arg[2]) : 2;             /* by a factor of 2. */

  size_t olen = (size_t)(AL(in) * orate / irate + .5);   /* Assay output len. */
  float * out = malloc(sizeof(*out) * olen);       /* Allocate output buffer. */
  size_t odone;

  soxr_error_t error = soxr_oneshot(irate, orate, 1, /* Rates and # of chans. */
      in, AL(in), NULL,                              /* Input. */
      out, olen, &odone,                             /* Output. */
      NULL, NULL, NULL);                             /* Default configuration.*/

  unsigned i = 0;                          /* Print out the resampled data... */
  while (i++ < odone)
    printf("%5.2f%c", out[i-1], " \n"[!(i&7) || i == odone]);
  puts(soxr_strerror(error));                  /* ...and the reported result. */

  free(out);                                                      /* Tidy up. */
  return !!error;
  (void)argc, (void)arg;                         /* Not used in this example. */
}
