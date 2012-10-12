/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 2b: resample a raw, single-channel, floating-point data stream from
 * stdin to stdout.  The application provides an input function, called on
 * demand by libsoxr, in response to calls to soxr_output().
 *
 * Arguments are: INPUT-RATE OUTPUT-RATE
 */

#include <soxr.h>
#include "examples-common.h"

#define iolen 10000

static size_t input_fn(
    void * p,           /* Generic pointer to any state variables neded here. */
    soxr_cbuf_t * buf,            /* To tell the resampler where the data is. */
    size_t requested_len)                          /* In samples per channel. */
{
  static float * ibuf;    /* Static context; this could be changed using `p'. */
  size_t actual;
  size_t len = min(requested_len, iolen);

  /* Allocate the input buffer memory; check for errors: */
  *buf = ibuf = (float *)realloc(ibuf, sizeof(float) * len);
  if (!ibuf)
    return 0;                           /* Indicate failure (*buf is also 0). */

  /* Read samples from the input stream; check for errors: */
  actual = fread(ibuf, sizeof(float), len, stdin);
  if (!actual) {
    if (ferror(stdin))
      *buf = 0;                       /* Indicate failure (actual is also 0). */
    free(ibuf), ibuf = 0;
  }

  return actual;
  (void)p;                                       /* Not used in this example. */
}

int main(int argc, char const * arg[])
{
  double irate = argc > 1? atof(arg[1]) : 44100.;
  double orate = argc > 2? atof(arg[2]) : 96000.;

  soxr_error_t error;
  soxr_t resampler = soxr_create(irate, orate, 1, &error, 0, 0, 0);
  if (!error)                        /* Register input_fn with the resampler: */
    error = soxr_set_input_fn(resampler, input_fn, 0);

  if (!error) {                         /* If all is good, run the resampler: */
    float resampled[iolen];
    size_t actual;
                                                       /* Resample in blocks: */
    USE_STD_STDIO;
    do actual = soxr_output(resampler, resampled, iolen);
    while (fwrite(resampled, sizeof(float), actual, stdout));

    error = soxr_error(resampler);    /* Note: before deleting the resampler! */
    soxr_delete(resampler);
  }
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; I/O: %s\n", arg[0],
      soxr_strerror(error), errno? strerror(errno) : "no error");
  return error || errno;
}
