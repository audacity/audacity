/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 3: extends example 2 with multiple channels, multiple datatypes,
 * and other options.
 *
 * The application provides an input function, called on demand by libsoxr, in
 * response to calls to soxr_output(); compared to the `process' approach
 * (illustrated in example 2) this requires that the application implements
 * less logic, but one more function.
 *
 * The 11 arguments (which are optional, from last to first) are:
 *   INPUT-RATE       As example 2
 *   OUTPUT-RATE      Ditto
 *   NUM-CHANNELS     Number of interleaved channels
 *   IN-DATATYPE#     0:float32 1:float64 2:int32 3:int16
 *   OUT-DATATYPE#    Ditto; or 11 for un-dithered int16
 *   Q-RECIPE         Quality recipe (in hex) See soxr.h
 *   Q-FLAGS          Quality flags  (in hex) See soxr.h
 *   PASSBAND-END     %
 *   STOPBAND-BEGIN   %
 *   PHASE-RESPONSE   [0,100]
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
  char const *     const arg0 = n? --n, *arg++ : "", * engine = "";
  double          const irate = n? --n, atof(*arg++) : 96000.;
  double          const orate = n? --n, atof(*arg++) : 44100.;
  unsigned        const chans = n? --n, (unsigned)atoi(*arg++) : 1;
  soxr_datatype_t const itype = n? --n, (soxr_datatype_t)atoi(*arg++) : 0;
  unsigned        const ospec = n? --n, (soxr_datatype_t)atoi(*arg++) : 0;
  unsigned long const q_recipe= n? --n, strtoul(*arg++, 0, 16) : SOXR_HQ;
  unsigned long const q_flags = n? --n, strtoul(*arg++, 0, 16) : 0;
  double   const passband_end = n? --n, atof(*arg++) : 0;
  double const stopband_begin = n? --n, atof(*arg++) : 0;
  double const phase_response = n? --n, atof(*arg++) : -1;
  int       const use_threads = n? --n, atoi(*arg++) : 1;
  soxr_datatype_t const otype = ospec & 3;

  soxr_quality_spec_t       q_spec = soxr_quality_spec(q_recipe, q_flags);
  soxr_io_spec_t            io_spec = soxr_io_spec(itype, otype);
  soxr_runtime_spec_t const runtime_spec = soxr_runtime_spec(!use_threads);

  /* Allocate resampling input and output buffers in proportion to the input
   * and output rates: */
  #define buf_total_len 15000  /* In samples per channel. */
  size_t const osize = soxr_datatype_size(otype) * chans;
  size_t const isize = soxr_datatype_size(itype) * chans;
  size_t const olen0= (size_t)(orate * buf_total_len / (irate + orate) + .5);
  size_t const olen = min(max(olen0, 1), buf_total_len - 1);
  size_t const ilen = buf_total_len - olen;
  void * const obuf = malloc(osize * olen);
  void * const ibuf = malloc(isize * ilen);

  input_context_t icontext;
  size_t odone, clips = 0;
  soxr_error_t error;
  soxr_t soxr;

  /* Overrides (if given): */
  if (passband_end   > 0) q_spec.passband_end   = passband_end / 100;
  if (stopband_begin > 0) q_spec.stopband_begin = stopband_begin / 100;
  if (phase_response >=0) q_spec.phase_response = phase_response;
  io_spec.flags = ospec & ~7u;

  /* Create a stream resampler: */
  soxr = soxr_create(
      irate, orate, chans,         /* Input rate, output rate, # of channels. */
      &error,                         /* To report any error during creation. */
      &io_spec, &q_spec, &runtime_spec);

  if (!error) {                      /* Register input_fn with the resampler: */
    icontext.ibuf = ibuf, icontext.isize = isize;
    error = soxr_set_input_fn(soxr, (soxr_input_fn_t)input_fn, &icontext, ilen);
  }

  if (!error) {                         /* If all is well, run the resampler: */
    engine = soxr_engine(soxr);
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
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s (%s)\n",
      arg0, soxr_strerror(error), (long unsigned)clips,
      ferror(stdin) || ferror(stdout)? strerror(errno) : "no error", engine);
  return !!error;
}
