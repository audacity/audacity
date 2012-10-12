/* SoX Resampler Library      Copyright (c) 2007-12 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Example 4: variant of example 3a; demonstrates I/O with split channels.
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
  soxr_io_spec_t io_spec = soxr_io_spec(itype|SOXR_SPLIT, otype|SOXR_SPLIT);
  soxr_runtime_spec_t runtime_spec = soxr_runtime_spec(!use_threads);

  soxr_t resampler = soxr_create(
      irate, orate, chans, &error, &io_spec, &q_spec, &runtime_spec);

  if (!error) {
    #define  buf_total_len 14000

    /* Allocate resampling input and output buffers in proportion to the input
     * and output rates: */
    size_t ibuflen = (size_t)(irate * buf_total_len / (irate + orate) + .5);
    size_t obuflen = buf_total_len - ibuflen;

    /* For split channels: */
    void * * ibuf_ptrs = malloc(sizeof(void *) * chans);
    void * * obuf_ptrs = malloc(sizeof(void *) * chans);
    char * * ibuf_offset_ptrs = malloc(sizeof(void *) * chans);
    char * ibufs = malloc(isize * ibuflen), * iptr = ibufs;
    char * obufs = malloc(osize * obuflen), * optr = obufs;

    /* For interleaved channels: */
    char * ibuf = malloc(isize * ibuflen);
    char * obuf = malloc(osize * obuflen);

    size_t iavailable = 0;
    size_t idone, odone, written;
    unsigned i;

    for (i = 0; i < chans; ++i) {
      ibuf_ptrs[i] = iptr;
      obuf_ptrs[i] = optr;
      iptr += ibuflen * soxr_datatype_size(itype);
      optr += obuflen * soxr_datatype_size(otype);
    }

    USE_STD_STDIO;
    do {
      if (!iavailable && ibuf_offset_ptrs) {     /* If ibuf empty, try to fill it: */
        if (!(iavailable = fread(ibuf, isize, ibuflen, stdin)))
          free(ibuf_offset_ptrs), ibuf_offset_ptrs = 0; /* If none available, don't retry. */
        else {
          memcpy(ibuf_offset_ptrs, ibuf_ptrs, sizeof(void *) * chans);
          deinterleave(itype, ibuf_ptrs, ibuf, iavailable, chans);
        }
      }

      error = soxr_process(resampler,
          ibuf_offset_ptrs, iavailable, &idone, obuf_ptrs, obuflen, &odone);

      if (ibuf_offset_ptrs) for (i = 0; i < chans; ++i)  /* Consumed input. */
        ibuf_offset_ptrs[i] += idone * isize;
      iavailable -= idone;

      interleave(otype, obuf, obuf_ptrs, odone, chans);  /* Consume output. */
      written = fwrite(obuf, osize, odone, stdout);
    } while (!error && (ibuf_offset_ptrs || written));

    free(obuf), free(ibuf), free(obufs), free(ibufs);
    free(obuf_ptrs), free(ibuf_ptrs), free(ibuf_offset_ptrs);
    clips = *soxr_num_clips(resampler);
    soxr_delete(resampler);
  }
  fprintf(stderr, "%-26s %s; %lu clips; I/O: %s\n", arg0, soxr_strerror(error),
      (long unsigned)clips, errno? strerror(errno) : "no error");
  return error || errno;
}
