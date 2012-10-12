/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 2a: resample a raw, single-channel, floating-point data stream from
 * stdin to stdout.  The application uses the single function `soxr_process'
 * for both input and output to/from the resampler.
 *
 * Arguments are: INPUT-RATE OUTPUT-RATE
 */

#include <soxr.h>
#include "examples-common.h"

int main(int argc, char const * arg[])
{
  double irate = argc > 1? atof(arg[1]) : 44100.;
  double orate = argc > 2? atof(arg[2]) : 96000.;

  soxr_error_t error;

  soxr_t resampler = soxr_create(
      irate, orate, 1,      /* Input rate, output rate, # of channels. */
      &error,               /* To report any error during creation. */
      NULL, NULL, NULL);    /* Use configuration defaults.*/

  if (!error) {
    #define buf_total_len 12000 /* In samples. */

    /* Allocate resampling input and output buffers in proportion to the input
     * and output rates: */
    size_t ibuflen = (size_t)(irate * buf_total_len / (irate + orate) + .5);
    size_t obuflen = buf_total_len - ibuflen;
    char * ibuf = malloc(sizeof(float) * ibuflen), * iptr = 0;
    void * obuf = malloc(sizeof(float) * obuflen);

    size_t iavailable = 0;
    size_t idone, odone, written;

    USE_STD_STDIO;
    do { /* Resample in blocks: */
      if (!iavailable && ibuf) {      /* If ibuf is empty, try to fill it: */
        iavailable = fread(ibuf, sizeof(float), ibuflen, stdin);
        if (!iavailable)        /* If none available, don't retry.  Pass NULL */
          free(ibuf), ibuf = 0;/* ibuf to resampler to indicate end-of-input. */
        iptr = ibuf;               /* Reset input to the start of the buffer. */
      }

      error = soxr_process(resampler,
          iptr, iavailable, &idone, obuf, obuflen, &odone);

      iptr += idone * sizeof(float);  /* Update input buffer according to how */
      iavailable -= idone;                /* much the resampler has consumed. */

      written = fwrite(obuf, sizeof(float), odone, stdout); /* Consume output.*/
    } while (!error && (ibuf || written));
                                                                  /* Tidy up: */
    free(obuf), free(ibuf);
    soxr_delete(resampler);
  }
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; I/O: %s\n", arg[0],
      soxr_strerror(error), errno? strerror(errno) : "no error");
  return error || errno;
}
