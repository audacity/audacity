/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 2: resample a raw, single-channel, floating-point data stream from
 * stdin to stdout.
 *
 * The application uses the single function `soxr_process' for both input and
 * output to/from the resampler; compared to the `input function' approach
 * (illustrated in example 3) this requires that the application implements
 * more logic, but one less function.
 *
 * Arguments are: INPUT-RATE OUTPUT-RATE
 */

#include <soxr.h>
#include "examples-common.h"

int main(int argc, char const * arg[])
{
  double const irate = argc > 1? atof(arg[1]) : 96000.;
  double const orate = argc > 2? atof(arg[2]) : 44100.;

  /* Allocate resampling input and output buffers in proportion to the input
   * and output rates: */
  #define buf_total_len 15000  /* In samples. */
  size_t const olen = (size_t)(orate * buf_total_len / (irate + orate) + .5);
  size_t const ilen = buf_total_len - olen;
  size_t const osize = sizeof(float), isize = osize;
  void * obuf = malloc(osize * olen);
  void * ibuf = malloc(isize * ilen);

  size_t odone, written, need_input = 1;
  soxr_error_t error;

  /* Create a stream resampler: */
  soxr_t soxr = soxr_create(
      irate, orate, 1,             /* Input rate, output rate, # of channels. */
      &error,                         /* To report any error during creation. */
      NULL, NULL, NULL);                        /* Use configuration defaults.*/

  if (!error) {                         /* If all is well, run the resampler: */
    USE_STD_STDIO;
                                                       /* Resample in blocks: */
    do {
      size_t ilen1 = 0;

      if (need_input) {

        /* Read one block into the buffer, ready to be resampled: */
        ilen1 = fread(ibuf, isize, ilen, stdin);

        if (!ilen1) {     /* If the is no (more) input data available, */
          free(ibuf);     /* set ibuf to NULL, to indicate end-of-input */
          ibuf = NULL;    /* to the resampler. */
        }
      }

      /* Copy data from the input buffer into the resampler, and resample
       * to produce as much output as is possible to the given output buffer: */
      error = soxr_process(soxr, ibuf, ilen1, NULL, obuf, olen, &odone);

      written = fwrite(obuf, osize, odone, stdout); /* Consume output.*/

      /* If the actual amount of data output is less than that requested, and
       * we have not already reached the end of the input data, then supply some
       * more input next time round the loop: */
      need_input = odone < olen && ibuf;

    } while (!error && (need_input || written));
  }
                                                                  /* Tidy up: */
  soxr_delete(soxr);
  free(obuf), free(ibuf);
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; I/O: %s\n", arg[0], soxr_strerror(error),
      ferror(stdin) || ferror(stdout)? strerror(errno) : "no error");
  return !!error;
}
