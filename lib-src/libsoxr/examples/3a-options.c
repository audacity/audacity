/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 3a: extends example 2a with multiple channels, multiple datatypes,
 * and other options.
 *
 * The eight arguments are:
 *   INPUT-RATE       As example 2a
 *   OUTPUT-RATE      Ditto
 *   NUM-CHANNELS     Number of interleaved channels
 *   IN-DATATYPE#     0:float32 1:float64 2:int32 3:int16
 *   OUT-DATATYPE#    Ditto
 *   Q-RECIPE         Quality recipe (in hex) See soxr.h
 *   Q-FLAGS          Quality flags  (in hex) See soxr.h
 *   USE-THREADS      1 to use multi-threading
 */

#include <soxr.h>
#include "examples-common.h"

int main(int n, char const * arg[])
{
  char const *     arg0 = n? --n, *arg++ : "";
  double          irate = n? --n, atof(*arg++) : 96000.;
  double          orate = n? --n, atof(*arg++) : 44100.;
  unsigned        chans = n? --n, (unsigned)atoi(*arg++) : 1;
  soxr_datatype_t itype = n? --n, (soxr_datatype_t)atoi(*arg++) :SOXR_FLOAT32_I;
  soxr_datatype_t otype = n? --n, (soxr_datatype_t)atoi(*arg++) :SOXR_FLOAT32_I;
  unsigned long q_recipe= n? --n, strtoul(*arg++, 0, 16) : SOXR_HQ;
  unsigned long q_flags = n? --n, strtoul(*arg++, 0, 16) : 0;
  int       use_threads = n? --n, atoi(*arg++) : 1;

  size_t isize = soxr_datatype_size(itype) * chans;
  size_t osize = soxr_datatype_size(otype) * chans;
  size_t clips = 0;
  soxr_error_t error;

  soxr_quality_spec_t q_spec = soxr_quality_spec(q_recipe, q_flags);
  soxr_io_spec_t     io_spec = soxr_io_spec(itype, otype);
  soxr_runtime_spec_t runtime_spec = soxr_runtime_spec(!use_threads);

  soxr_t resampler = soxr_create(
      irate, orate, chans, &error, &io_spec, &q_spec, &runtime_spec);

  if (!error) {
    #define  buf_total_len 15000 /* In samples. */

    /* Allocate resampling input and output buffers in proportion to the input
     * and output rates: */
    size_t ibuflen = (size_t)(irate * buf_total_len / (irate + orate) + .5);
    size_t obuflen = buf_total_len - ibuflen;
    char * ibuf = malloc(isize * ibuflen), * iptr = 0;
    void * obuf = malloc(osize * obuflen);

    size_t iavailable = 0;
    size_t idone, odone, written;

    USE_STD_STDIO;
    do { /* Resample in blocks: */
      if (!iavailable && ibuf) {      /* If ibuf is empty, try to fill it: */
        iavailable = fread(ibuf, isize, ibuflen, stdin);
        if (!iavailable)        /* If none available, don't retry.  Pass NULL */
          free(ibuf), ibuf = 0;/* ibuf to resampler to indicate end-of-input. */
        iptr = ibuf;               /* Reset input to the start of the buffer. */
      }

      error = soxr_process(resampler,
          iptr, iavailable, &idone, obuf, obuflen, &odone);

      iptr += idone * isize;          /* Update input buffer according to how */
      iavailable -= idone;                /* much the resampler has consumed. */

      written = fwrite(obuf, osize, odone, stdout);  /* Consume output. */
    } while (!error && (ibuf || written));

    free(obuf), free(ibuf);
    clips = *soxr_num_clips(resampler); /* Can occur only with integer output.*/
    soxr_delete(resampler);
  }
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s\n", arg0, soxr_strerror(error),
      (long unsigned)clips, errno? strerror(errno) : "no error");
  return error || errno;
}
