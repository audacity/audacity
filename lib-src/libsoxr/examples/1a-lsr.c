/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 1a: Variant of example 1 using libsamplerate-like bindings. */

#include <soxr-lsr.h>
#include "examples-common.h"

float in[] = {  /* Input: 12 cycles of a sine wave with freq. = irate/4 */
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1,
  0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1, 0,1,0,-1};

int main(int argc, char const * arg[])
{
  double irate = argc > 1? atof(arg[1]) : 1;      /* Default to interpolation */
  double orate = argc > 2? atof(arg[2]) : 2;             /* by a factor of 2. */

  size_t olen = (size_t)(AL(in) * orate / irate + .5);   /* Assay output len. */
  float * out = (float *)malloc(sizeof(*out) * olen); /* Allocate output buf. */

  int error, i = 0;
  SRC_DATA data;

  data.data_in = in;
  data.data_out = out;
  data.input_frames = AL(in);
  data.output_frames = (int)olen;
  data.src_ratio = orate / irate;
  error = src_simple(&data, SRC_SINC_FASTEST, 1);

  while (i++ < data.output_frames_gen)       /* Print out the resampled data, */
    printf("%5.2f%c", out[i-1], " \n"[!(i&7) || i == data.output_frames_gen]);
  printf("%-26s %s\n", arg[0], src_strerror(error));  /* and reported result. */

  if (argc > 3)                                     /* Library version check: */
    printf("runtime=%s\n", src_get_version());

  free(out);                                                      /* Tidy up. */
  return !!error;
}
