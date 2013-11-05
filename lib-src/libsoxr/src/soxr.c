/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "soxr.h"
#include "data-io.h"
#include "internal.h"



char const * soxr_version(void)
{
  return "libsoxr-" SOXR_THIS_VERSION_STR;
}



typedef void sample_t; /* float or double */
typedef void (* fn_t)(void);
typedef fn_t control_block_t[10];

#define resampler_input        (*(sample_t * (*)(void *, sample_t * samples, size_t   n))p->control_block[0])
#define resampler_process      (*(void (*)(void *, size_t))p->control_block[1])
#define resampler_output       (*(sample_t const * (*)(void *, sample_t * samples, size_t * n))p->control_block[2])
#define resampler_flush        (*(void (*)(void *))p->control_block[3])
#define resampler_close        (*(void (*)(void *))p->control_block[4])
#define resampler_delay        (*(double (*)(void *))p->control_block[5])
#define resampler_sizes        (*(void (*)(size_t * shared, size_t * channel))p->control_block[6])
#define resampler_create       (*(char const * (*)(void * channel, void * shared, double io_ratio, soxr_quality_spec_t * q_spec, soxr_runtime_spec_t * r_spec, double scale))p->control_block[7])
#define resampler_set_io_ratio (*(void (*)(void *, double io_ratio, size_t len))p->control_block[8])
#define resampler_id           (*(char const * (*)(void))p->control_block[9])

typedef void * resampler_t; /* For one channel. */
typedef void * resampler_shared_t; /* Between channels. */
typedef void (* deinterleave_t)(sample_t * * dest,
    soxr_datatype_t data_type, void const * * src0, size_t n, unsigned ch);
typedef size_t (* interleave_t)(soxr_datatype_t data_type, void * * dest,
    sample_t const * const * src, size_t, unsigned, unsigned long *);

struct soxr {
  unsigned num_channels;
  double io_ratio;
  soxr_error_t error;
  soxr_quality_spec_t q_spec;
  soxr_io_spec_t io_spec;
  soxr_runtime_spec_t runtime_spec;

  void * input_fn_state;
  soxr_input_fn_t input_fn;
  size_t max_ilen;

  resampler_shared_t shared;
  resampler_t * resamplers;
  control_block_t control_block;
  deinterleave_t deinterleave;
  interleave_t interleave;

  void * * channel_ptrs;
  size_t clips;
  unsigned long seed;
  int flushing;
};



/* TODO: these should not be here. */
#define TO_3dB(a)       ((1.6e-6*a-7.5e-4)*a+.646)
#define LOW_Q_BW0       (1385 / 2048.) /* 0.67625 rounded to be a FP exact. */

soxr_quality_spec_t soxr_quality_spec(unsigned long recipe, unsigned long flags)
{
  soxr_quality_spec_t spec, * p = &spec;
  unsigned quality = recipe & 0xf;
  double rej;
  memset(p, 0, sizeof(*p));
  if (quality > 13) {
    p->e = "invalid quality type";
    return spec;
  }
  if (quality == 13)
    quality = 6;
  else if (quality > 10)
    quality = 0;
  p->phase_response = "\62\31\144"[(recipe & 0x30)>>8];
  p->stopband_begin = 1;
  p->precision = !quality? 0: quality < 3? 16 : quality < 8? 4 + quality * 4 : 55 - quality * 4;
  rej = p->precision * linear_to_dB(2.);
  p->flags = flags;
  if (quality < 8) {
    p->passband_end = quality == 1? LOW_Q_BW0 : 1 - .05 / TO_3dB(rej);
    if (quality <= 2)
      p->flags &= ~SOXR_ROLLOFF_NONE, p->flags |= SOXR_ROLLOFF_MEDIUM;
  }
  else {
    static float const bw[] = {.931f, .832f, .663f};
    p->passband_end = bw[quality - 8];
    if (quality - 8 == 2)
      p->flags &= ~SOXR_ROLLOFF_NONE, p->flags |= SOXR_ROLLOFF_MEDIUM;
  }
  if (recipe & SOXR_STEEP_FILTER)
    p->passband_end = 1 - .01 / TO_3dB(rej);
  return spec;
}



char const * soxr_engine(soxr_t p)
{
  return resampler_id();
}



size_t * soxr_num_clips(soxr_t p)
{
  return &p->clips;
}



soxr_error_t soxr_error(soxr_t p)
{
  return p->error;
}



soxr_runtime_spec_t soxr_runtime_spec(unsigned num_threads)
{
  soxr_runtime_spec_t spec, * p = &spec;
  memset(p, 0, sizeof(*p));
  p->log2_min_dft_size = 10;
  p->log2_large_dft_size = 17;
  p->coef_size_kbytes = 400;
  p->num_threads = num_threads;
  return spec;
}



soxr_io_spec_t soxr_io_spec(
  soxr_datatype_t itype,
  soxr_datatype_t otype)
{
  soxr_io_spec_t spec, * p = &spec;
  memset(p, 0, sizeof(*p));
  if ((itype | otype) >= SOXR_SPLIT * 2)
    p->e = "invalid io datatype(s)";
  else {
    p->itype = itype;
    p->otype = otype;
    p->scale = 1;
  }
  return spec;
}



#if HAVE_SIMD
static bool cpu_has_simd(void)
{
#if defined __x86_64__ || defined _M_X64
  return true;
#elif defined __GNUC__ && defined i386
  uint32_t eax, ebx, ecx, edx;
  __asm__ __volatile__ (
      "pushl %%ebx   \n\t"
      "cpuid         \n\t"
      "movl %%ebx, %1\n\t"
      "popl %%ebx    \n\t"
      : "=a"(eax), "=r"(ebx), "=c"(ecx), "=d"(edx)
      : "a"(1)
      : "cc" );
  return !!(edx & 0x06000000);
#elif defined _MSC_VER && defined _M_IX86
  uint32_t d;
  __asm {
    xor     eax, eax
    inc     eax
    push    ebx
    cpuid
    pop     ebx
    mov     d, edx
  }
  return !!(d & 0x06000000);
#endif
  return false;
}
#endif

extern control_block_t _soxr_rate32s_cb, _soxr_rate32_cb, _soxr_rate64_cb, _soxr_vr32_cb;



soxr_t soxr_create(
  double input_rate, double output_rate,
  unsigned num_channels,
  soxr_error_t * error0,
  soxr_io_spec_t const * io_spec,
  soxr_quality_spec_t const * q_spec,
  soxr_runtime_spec_t const * runtime_spec)
{
  double io_ratio = output_rate? input_rate? input_rate / output_rate : -1 : input_rate? -1 : 0;
  static const float datatype_full_scale[] = {1, 1, 65536.*32768, 32768};
  soxr_t p = 0;
  soxr_error_t error = 0;

  if (q_spec && q_spec->e)  error = q_spec->e;
  else if (io_spec && (io_spec->itype | io_spec->otype) >= SOXR_SPLIT * 2)
    error = "invalid io datatype(s)";

  if (!error && !(p = calloc(sizeof(*p), 1))) error = "malloc failed";

  if (p) {
    p->q_spec = q_spec? *q_spec : soxr_quality_spec(SOXR_HQ, 0);

    if (q_spec) { /* Backwards compatibility with original API: */
      if (p->q_spec.passband_end > 2)
        p->q_spec.passband_end /= 100;
      if (p->q_spec.stopband_begin > 2)
        p->q_spec.stopband_begin = 2 - p->q_spec.stopband_begin / 100;
    }

    p->io_ratio = io_ratio;
    p->num_channels = num_channels;
    if (io_spec)
      p->io_spec = *io_spec;
    else
      p->io_spec.scale = 1;

    p->runtime_spec = runtime_spec? *runtime_spec : soxr_runtime_spec(1);
    p->io_spec.scale *= datatype_full_scale[p->io_spec.otype & 3] /
                        datatype_full_scale[p->io_spec.itype & 3];
    p->seed = (unsigned long)time(0) ^ (unsigned long)(size_t)p;

#if HAVE_SINGLE_PRECISION
    if (!HAVE_DOUBLE_PRECISION || (p->q_spec.precision <= 20 && !(p->q_spec.flags & SOXR_DOUBLE_PRECISION))
        || (p->q_spec.flags & SOXR_VR)) {
      p->deinterleave = (deinterleave_t)_soxr_deinterleave_f;
      p->interleave = (interleave_t)_soxr_interleave_f;
      memcpy(&p->control_block,
          (p->q_spec.flags & SOXR_VR)? &_soxr_vr32_cb :
#if HAVE_SIMD
          cpu_has_simd()? &_soxr_rate32s_cb :
#endif
          &_soxr_rate32_cb, sizeof(p->control_block));
    }
#if HAVE_DOUBLE_PRECISION
    else
#endif
#endif
#if HAVE_DOUBLE_PRECISION
    {
      p->deinterleave = (deinterleave_t)_soxr_deinterleave;
      p->interleave = (interleave_t)_soxr_interleave;
      memcpy(&p->control_block, &_soxr_rate64_cb, sizeof(p->control_block));
    }
#endif

    if (p->num_channels && io_ratio)
      error = soxr_set_io_ratio(p, io_ratio, 0);
  }
  if (error)
    soxr_delete(p), p = 0;
  if (error0)
    *error0 = error;
  return p;
}



soxr_error_t soxr_set_input_fn(soxr_t p,
    soxr_input_fn_t input_fn, void * input_fn_state, size_t max_ilen)
{
  p->input_fn_state = input_fn_state;
  p->input_fn = input_fn;
  p->max_ilen = max_ilen? max_ilen : (size_t)-1;
  return 0;
}



static void soxr_delete0(soxr_t p)
{
  unsigned i;

  if (p->resamplers) for (i = 0; i < p->num_channels; ++i) {
    if (p->resamplers[i])
      resampler_close(p->resamplers[i]);
    free(p->resamplers[i]);
  }
  free(p->resamplers);
  free(p->channel_ptrs);
  free(p->shared);

  memset(p, 0, sizeof(*p));
}



double soxr_delay(soxr_t p)
{
  return (p && !p->error && p->resamplers)? resampler_delay(p->resamplers[0]) : 0;
}



static soxr_error_t fatal_error(soxr_t p, soxr_error_t error)
{
  soxr_delete0(p);
  return p->error = error;
}



static soxr_error_t initialise(soxr_t p)
{
  unsigned i;
  size_t shared_size, channel_size;

  resampler_sizes(&shared_size, &channel_size);
  p->channel_ptrs = calloc(sizeof(*p->channel_ptrs), p->num_channels);
  p->shared = calloc(shared_size, 1);
  p->resamplers = calloc(sizeof(*p->resamplers), p->num_channels);
  if (!p->shared || !p->channel_ptrs || !p->resamplers)
    return fatal_error(p, "malloc failed");

  for (i = 0; i < p->num_channels; ++i) {
    soxr_error_t error;
    if (!(p->resamplers[i] = calloc(channel_size, 1)))
      return fatal_error(p, "malloc failed");
    error = resampler_create(
        p->resamplers[i],
        p->shared,
        p->io_ratio,
        &p->q_spec,
        &p->runtime_spec,
        p->io_spec.scale);
    if (error)
      return fatal_error(p, error);
  }
  return 0;
}



soxr_error_t soxr_set_num_channels(soxr_t p, unsigned num_channels)
{
  if (!p)                return "invalid soxr_t pointer";
  if (num_channels == p->num_channels) return p->error;
  if (!num_channels)     return "invalid # of channels";
  if (p->resamplers)     return "# of channels can't be changed";
  p->num_channels = num_channels;
  return soxr_set_io_ratio(p, p->io_ratio, 0);
}



soxr_error_t soxr_set_io_ratio(soxr_t p, double io_ratio, size_t slew_len)
{
  unsigned i;
  soxr_error_t error;
  if (!p)                 return "invalid soxr_t pointer";
  if ((error = p->error)) return error;
  if (!p->num_channels)   return "must set # channels before O/I ratio";
  if (io_ratio <= 0)      return "I/O ratio out-of-range";
  if (!p->channel_ptrs) {
    p->io_ratio = io_ratio;
    return initialise(p);
  }
  if (p->control_block[8]) {
    for (i = 0; !error && i < p->num_channels; ++i)
      resampler_set_io_ratio(p->resamplers[i], io_ratio, slew_len);
    return error;
  }
  return fabs(p->io_ratio - io_ratio) < 1e-15? 0 :
    "Varying O/I ratio is not supported with this quality level";
}



void soxr_delete(soxr_t p)
{
  if (p)
    soxr_delete0(p), free(p);
}



soxr_error_t soxr_clear(soxr_t p) /* TODO: this, properly. */
{
  if (p) {
    struct soxr tmp = *p;
    soxr_delete0(p);
    memset(p, 0, sizeof(*p));
    p->input_fn = tmp.input_fn;
    p->runtime_spec = tmp.runtime_spec;
    p->q_spec = tmp.q_spec;
    p->io_spec = tmp.io_spec;
    p->num_channels = tmp.num_channels;
    p->input_fn_state = tmp.input_fn_state;
    memcpy(p->control_block, tmp.control_block, sizeof(p->control_block));
    p->deinterleave = tmp.deinterleave;
    p->interleave = tmp.interleave;
    return 0;
  }
  return "invalid soxr_t pointer";
}



static void soxr_input_1ch(soxr_t p, unsigned i, soxr_cbuf_t src, size_t len)
{
  sample_t * dest = resampler_input(p->resamplers[i], NULL, len);
  (*p->deinterleave)(&dest, p->io_spec.itype, &src, len, 1);
}



static size_t soxr_input(soxr_t p, void const * in, size_t len)
{
  bool separated = !!(p->io_spec.itype & SOXR_SPLIT);
  unsigned i;
  if (!p || p->error) return 0;
  if (!in && len) {p->error = "null input buffer pointer"; return 0;}
  if (!len) {
    p->flushing = true;
    return 0;
  }
  if (separated)
    for (i = 0; i < p->num_channels; ++i)
      soxr_input_1ch(p, i, ((soxr_cbufs_t)in)[i], len);
  else {
    for (i = 0; i < p->num_channels; ++i)
      p->channel_ptrs[i] = resampler_input(p->resamplers[i], NULL, len);
    (*p->deinterleave)(
        (sample_t **)p->channel_ptrs, p->io_spec.itype, &in, len, p->num_channels);
  }
  return len;
}



static size_t soxr_output_1ch(soxr_t p, unsigned i, soxr_buf_t dest, size_t len, bool separated)
{
  sample_t const * src;
  if (p->flushing)
    resampler_flush(p->resamplers[i]);
  resampler_process(p->resamplers[i], len);
  src = resampler_output(p->resamplers[i], NULL, &len);
  if (separated)
    p->clips += (p->interleave)(p->io_spec.otype, &dest, &src,
      len, 1, (p->io_spec.flags & SOXR_NO_DITHER)? 0 : &p->seed);
  else p->channel_ptrs[i] = (void /* const */ *)src;
  return len;
}



static size_t soxr_output_no_callback(soxr_t p, soxr_buf_t out, size_t len)
{
  unsigned u;
  size_t done = 0;
  bool separated = !!(p->io_spec.otype & SOXR_SPLIT);
#if defined _OPENMP
  int i;
  if (!p->runtime_spec.num_threads && p->num_channels > 1)
#pragma omp parallel for
  for (i = 0; i < (int)p->num_channels; ++i) {
    size_t done1;
    done1 = soxr_output_1ch(p, (unsigned)i, ((soxr_bufs_t)out)[i], len, separated);
    if (!i)
      done = done1;
  } else
#endif
  for (u = 0; u < p->num_channels; ++u)
    done = soxr_output_1ch(p, u, ((soxr_bufs_t)out)[u], len, separated);

  if (!separated)
    p->clips += (p->interleave)(p->io_spec.otype, &out, (sample_t const * const *)p->channel_ptrs,
        done, p->num_channels, (p->io_spec.flags & SOXR_NO_DITHER)? 0 : &p->seed);
  return done;
}



size_t soxr_output(soxr_t p, void * out, size_t len0)
{
  size_t odone, odone0 = 0, olen = len0, osize, idone;
  size_t ilen = min(p->max_ilen, (size_t)ceil((double)olen *p->io_ratio));
  void const * in = out; /* Set to !=0, so that caller may leave unset. */
  bool was_flushing;

  if (!p || p->error) return 0;
  if (!out && len0) {p->error = "null output buffer pointer"; return 0;}

  do {
    odone = soxr_output_no_callback(p, out, olen);
    odone0 += odone;
    if (odone0 == len0 || !p->input_fn || p->flushing)
      break;

    osize = soxr_datatype_size(p->io_spec.otype) * p->num_channels;
    out = (char *)out + osize * odone;
    olen -= odone;
    idone = p->input_fn(p->input_fn_state, &in, ilen);
    was_flushing = p->flushing;
    if (!in)
      p->error = "input function reported failure";
    else soxr_input(p, in, idone);
  } while (odone || idone || (!was_flushing && p->flushing));
  return odone0;
}



static size_t soxr_i_for_o(soxr_t p, size_t olen, size_t ilen)
{
  size_t result;
#if 0
  if (p->runtime_spec.flags & SOXR_STRICT_BUFFERING)
    result = rate_i_for_o(p->resamplers[0], olen);
  else
#endif
    result = (size_t)ceil((double)olen * p->io_ratio);
  return min(result, ilen);
}



#if 0
static size_t soxr_o_for_i(soxr_t p, size_t ilen, size_t olen)
{
  size_t result = (size_t)ceil((double)ilen / p->io_ratio);
  return min(result, olen);
}
#endif



soxr_error_t soxr_process(soxr_t p,
    void const * in , size_t ilen0, size_t * idone0,
    void       * out, size_t olen , size_t * odone0)
{
  size_t ilen, idone, odone = 0;
  unsigned u;
  bool flush_requested = false;

  if (!p) return "null pointer";

  if (!in)
    flush_requested = true, ilen = ilen0 = 0;
  else {
    if ((ptrdiff_t)ilen0 < 0)
      flush_requested = true, ilen0 = ~ilen0;
    if (idone0 && (1 || flush_requested))
      ilen = soxr_i_for_o(p, olen, ilen0);
    else
      ilen = ilen0/*, olen = soxr_o_for_i(p, ilen, olen)*/;
  }
  p->flushing |= ilen == ilen0 && flush_requested;

  if (!out && !in)
    idone = ilen;
  else if (p->io_spec.itype & p->io_spec.otype & SOXR_SPLIT) { /* Both i & o */
#if defined _OPENMP
    int i;
    if (!p->runtime_spec.num_threads && p->num_channels > 1)
#pragma omp parallel for
    for (i = 0; i < (int)p->num_channels; ++i) {
      size_t done;
      if (in)
        soxr_input_1ch(p, (unsigned)i, ((soxr_cbufs_t)in)[i], ilen);
      done = soxr_output_1ch(p, (unsigned)i, ((soxr_bufs_t)out)[i], olen, true);
      if (!i)
        odone = done;
    } else
#endif
    for (u = 0; u < p->num_channels; ++u) {
      if (in)
        soxr_input_1ch(p, u, ((soxr_cbufs_t)in)[u], ilen);
      odone = soxr_output_1ch(p, u, ((soxr_bufs_t)out)[u], olen, true);
    }
    idone = ilen;
  }
  else {
    idone = ilen? soxr_input (p, in , ilen) : 0;
    odone = soxr_output(p, out, olen);
  }
  if (idone0) *idone0 = idone;
  if (odone0) *odone0 = odone;
  return p->error;
}



soxr_error_t soxr_oneshot(
    double irate, double orate,
    unsigned num_channels,
    void const * in , size_t ilen, size_t * idone,
    void * out, size_t olen, size_t * odone,
    soxr_io_spec_t const * io_spec,
    soxr_quality_spec_t const * q_spec,
    soxr_runtime_spec_t const * runtime_spec)
{
  soxr_t resampler;
  soxr_error_t error = q_spec? q_spec->e : 0;
  if (!error) {
    soxr_quality_spec_t q_spec1;
    if (!q_spec)
      q_spec1 = soxr_quality_spec(SOXR_LQ, 0), q_spec = &q_spec1;
    resampler = soxr_create(irate, orate, num_channels,
        &error, io_spec, q_spec, runtime_spec);
  }
  if (!error) {
    error = soxr_process(resampler, in, ~ilen, idone, out, olen, odone);
    soxr_delete(resampler);
  }
  return error;
}



soxr_error_t soxr_set_error(soxr_t p, soxr_error_t error)
{
  if (!p) return "null pointer";
  if (!p->error && p->error != error) return p->error;
  p->error = error;
  return 0;
}
