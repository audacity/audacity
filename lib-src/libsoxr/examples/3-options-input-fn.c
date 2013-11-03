/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 3: extends example 2 with multiple channels, multiple datatypes,
 * and other options.
 *
 * The application provides an input function, called on demand by libsoxr, in
 * response to calls to soxr_output(); compared to the `process' approach
 * (illustrated in example 2) this requires that the application implements
 * less logic, but one more function.
 *
 * The eight arguments (which are optional, from last to first) are:
 *   INPUT-RATE       As example 2
 *   OUTPUT-RATE      Ditto
 *   NUM-CHANNELS     Number of interleaved channels
 *   IN-DATATYPE#     0:float32 1:float64 2:int32 3:int16
 *   OUT-DATATYPE#    Ditto
 *   Q-RECIPE         Quality recipe (in hex) See soxr.h
 *   Q-FLAGS          Quality flags  (in hex) See soxr.h
 *   USE-THREADS      1 to use multi-threading (where available)
 */

#include <soxr.h>
#include "examples-common.h"

typedef struct {void * ibuf; size_t isize;} input_context_t;

static size_t input_fn(input_context_t * p, soxr_cbuf_t * buf, size_t len)
{
  /* Read one block into the buffer, ready to be input to the resampler: */
  len = fread(p->ibuf, p->isize, len, stdin); /* Actual len read may be less. */

  /* Inform the resampler of the data's whereabouts (which could be anywhere, in
   * a freshly malloc'd buffer, for example): */
  *buf = (!len && ferror(stdin))? NULL : p->ibuf;  /* NULL if error occurred. */

  return len;                           /* # of samples per channel to input. */
}

int main(int n, char const * arg[])
{
  char const *     const arg0 = n? --n, *arg++ : "";
  double          const irate = n? --n, atof(*arg++) : 96000.;
  double          const orate = n? --n, atof(*arg++) : 44100.;
  unsigned        const chans = n? --n, (unsigned)atoi(*arg++) : 1;
  soxr_datatype_t const itype = n? --n, (soxr_datatype_t)atoi(*arg++) : 0;
  soxr_datatype_t const otype = n? --n, (soxr_datatype_t)atoi(*arg++) : 0;
  unsigned long const q_recipe= n? --n, strtoul(*arg++, 0, 16) : SOXR_HQ;
  unsigned long const q_flags = n? --n, strtoul(*arg++, 0, 16) : 0;
  int       const use_threads = n? --n, atoi(*arg++) : 1;

  soxr_quality_spec_t const q_spec = soxr_quality_spec(q_recipe, q_flags);
  soxr_io_spec_t      const io_spec = soxr_io_spec(itype, otype);
  soxr_runtime_spec_t const runtime_spec = soxr_runtime_spec(!use_threads);

  /* Allocate resampling input and output buffers in proportion to the input
   * and output rates: */
  #define buf_total_len 15000  /* In samples per channel. */
  size_t const osize = soxr_datatype_size(otype) * chans;
  size_t const isize = soxr_datatype_size(itype) * chans;
  size_t const olen = (size_t)(orate * buf_total_len / (irate + orate) + .5);
  size_t const ilen = buf_total_len - olen;
  void * const obuf = malloc(osize * olen);
  void * const ibuf = malloc(isize * ilen);

  input_context_t icontext;
  size_t odone, clips = 0;
  soxr_error_t error;

  /* Create a stream resampler: */
  soxr_t soxr = soxr_create(
      irate, orate, chans,         /* Input rate, output rate, # of channels. */
      &error,                         /* To report any error during creation. */
      &io_spec, &q_spec, &runtime_spec);

  if (!error) {                      /* Register input_fn with the resampler: */
    icontext.ibuf = ibuf, icontext.isize = isize;
    error = soxr_set_input_fn(soxr, (soxr_input_fn_t)input_fn, &icontext, ilen);
  }

  if (!error) {                         /* If all is well, run the resampler: */
    USE_STD_STDIO;
                                                       /* Resample in blocks: */
    do odone = soxr_output(soxr, obuf, olen);
    while (fwrite(obuf, osize, odone, stdout));            /* Consume output. */

    error = soxr_error(soxr);            /* Check if any soxr error occurred. */
    clips = *soxr_num_clips(soxr);     /* Can occur only with integer output. */
  }
                                                                  /* Tidy up: */
  soxr_delete(soxr);
  free(obuf), free(ibuf);
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s\n", arg0, soxr_strerror(error),
      (long unsigned)clips, errno? strerror(errno) : "no error");
  return error || errno;
}
