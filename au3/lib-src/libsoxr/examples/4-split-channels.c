/* SoX Resampler Library      Copyright (c) 2007-18 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 4: variant of examples 2 & 3, demonstrating I/O with split channels.
 *
 * Note that, for convenience of the demonstration, split-channel data is
 * made available by deinterleaving data sourced from and sent to
 * interleaved file-streams; this adds a lot of code to the example that,
 * for purposes of understanding how to use split-channels, may safely be
 * ignored.  In a real application, the channel-data might never be
 * interleaved; for example, the split-channel data output from the
 * resampler might be sent directly to digital-to-analogue converters.
 *
 * Note also (not shown in the examples) that split/interleaved channels may
 * be used for input and output independently.
 *
 * Arguments are as example 3.
 */

#include <soxr.h>
#include "examples-common.h"



#define DEINTERLEAVE(T) do { \
  unsigned i; \
  size_t j; \
  T * const * dest = (T * const *)dest0; \
  T const * src = src0; \
  if (ch == 1) memcpy(dest[0], src, n * sizeof(dest[0][0])); \
  else for (j = 0; j < n; ++j) for (i = 0; i < ch; ++i) dest[i][j] = *src++; \
  return; \
} while (0)

static void deinterleave(soxr_datatype_t data_type,
    void * const * dest0,
    void const * src0,
    size_t n, unsigned ch)
{
  switch (data_type & 3) {
    case SOXR_FLOAT32: DEINTERLEAVE(float);
    case SOXR_FLOAT64: DEINTERLEAVE(double);
    case SOXR_INT32  : DEINTERLEAVE(int32_t);
    case SOXR_INT16  : DEINTERLEAVE(int16_t);
    default: break;
  }
}

#define INTERLEAVE(T) do { \
  unsigned i; \
  size_t j; \
  T * dest = dest0; \
  T const * const * src = (T const * const *)src0; \
  if (ch == 1) memcpy(dest, src[0], n * sizeof(dest[0])); \
  else for (j = 0; j < n; ++j) for (i = 0; i < ch; ++i) *dest++ = src[i][j]; \
  return; \
} while (0)

static void interleave(soxr_datatype_t data_type, void * dest0,
  void * const * src0, size_t n, unsigned ch)
{
  switch (data_type & 3) {
    case SOXR_FLOAT32: INTERLEAVE(float);
    case SOXR_FLOAT64: INTERLEAVE(double);
    case SOXR_INT32  : INTERLEAVE(int32_t);
    case SOXR_INT16  : INTERLEAVE(int16_t);
    default: break;
  }
}

int main(int n, char const * arg[])
{
  char const *     const arg0 = n? --n, *arg++ : "";
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

  soxr_quality_spec_t  q_spec = soxr_quality_spec(q_recipe, q_flags);
  soxr_io_spec_t       io_spec=soxr_io_spec(itype|SOXR_SPLIT, otype|SOXR_SPLIT);
  soxr_runtime_spec_t const runtime_spec = soxr_runtime_spec(!use_threads);

  /* Allocate resampling input and output buffers in proportion to the input
   * and output rates: */
  #define buf_total_len 15000  /* In samples per channel. */
  size_t const osize = soxr_datatype_size(otype) * chans;
  size_t const isize = soxr_datatype_size(itype) * chans;
  size_t const olen = (size_t)(orate * buf_total_len / (irate + orate) + .5);
  size_t const ilen = buf_total_len - olen;

  /* For split channels: */
  void * * const obuf_ptrs = malloc(sizeof(void *) * chans);
  void * *       ibuf_ptrs = malloc(sizeof(void *) * chans);
  char * const obufs = malloc(osize * olen), * optr = obufs;
  char * const ibufs = malloc(isize * ilen), * iptr = ibufs;

  /* For interleaved channels: */
  char * const obuf = malloc(osize * olen);
  char * const ibuf = malloc(isize * ilen);

  size_t odone, written, need_input = 1, clips = 0;
  soxr_error_t error;
  soxr_t soxr;
  unsigned i;

  /* Overrides (if given): */
  if (passband_end   > 0) q_spec.passband_end   = passband_end / 100;
  if (stopband_begin > 0) q_spec.stopband_begin = stopband_begin / 100;
  if (phase_response >=0) q_spec.phase_response = phase_response;
  io_spec.flags = ospec & ~7u;

  soxr = soxr_create(
      irate, orate, chans, &error, &io_spec, &q_spec, &runtime_spec);

  for (i = 0; i < chans; ++i) {
    ibuf_ptrs[i] = iptr;
    obuf_ptrs[i] = optr;
    iptr += ilen * soxr_datatype_size(itype);
    optr += olen * soxr_datatype_size(otype);
  }

  if (!error) {
    USE_STD_STDIO;

    do {
      size_t ilen1 = 0;

      if (need_input) {
        if (!(ilen1 = fread(ibuf, isize, ilen, stdin)))
          free(ibuf_ptrs), ibuf_ptrs = 0; /* If none available, don't retry. */
        else deinterleave(itype, ibuf_ptrs, ibuf, ilen1, chans);
      }

      error = soxr_process(soxr, ibuf_ptrs, ilen1, NULL, obuf_ptrs, olen, &odone);
      interleave(otype, obuf, obuf_ptrs, odone, chans);  /* Consume output... */
      written = fwrite(obuf, osize, odone, stdout);

      need_input = odone < olen && ibuf_ptrs;

    } while (!error && (need_input || written));

    clips = *soxr_num_clips(soxr);     /* Can occur only with integer output. */
  }
                                                                  /* Tidy up: */
  soxr_delete(soxr);
  free(obuf), free(ibuf), free(obufs), free(ibufs);
  free(obuf_ptrs), free(ibuf_ptrs);
                                                              /* Diagnostics: */
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s\n",
      arg0, soxr_strerror(error), (long unsigned)clips,
      ferror(stdin) || ferror(stdout)? strerror(errno) : "no error");
  return !!error;
}
