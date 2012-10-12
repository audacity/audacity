/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 3b: extends example 2b with multiple channels, multiple datatypes,
 * and other options.
 *
 * The eight arguments are:
 *   INPUT-RATE       As example 2b
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

#define iolen 8000



static size_t input_fn(void * p, soxr_cbuf_t * buf, size_t len)
{
  static float * ibuf;
  size_t isize = *(size_t *)p;
  len = min(len, iolen);
  *buf = ibuf = realloc(ibuf, isize * len);
  if (!ibuf)
    return 0;
  len = fread(ibuf, isize, len, stdin);
  if (!len) {
    if (ferror(stdin))
      *buf = 0;
    free(ibuf), ibuf = 0;
  }
  return len;
}



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
  if (!error) error = soxr_set_input_fn(resampler, input_fn, &isize);

  USE_STD_STDIO;
  if (!error) {
    void * resampled = malloc(osize * iolen);
    size_t actual;

    do actual = soxr_output(resampler, resampled, iolen);
    while (fwrite(resampled, osize, actual, stdout));

    free(resampled);
    error = soxr_error(resampler);
    clips = *soxr_num_clips(resampler); /* Can occur only with integer output.*/
    soxr_delete(resampler);
  }
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s\n", arg0, soxr_strerror(error),
      (long unsigned)clips, errno? strerror(errno) : "no error");
  return error || errno;
}
