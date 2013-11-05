/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

/* Wrapper mostly compatible with `libsamplerate'. */

#include <assert.h>
#include <stdlib.h>
#include "soxr.h"

/* Runtime casts: */
typedef struct io_t {
  float *in,*out; long ilen,olen,idone,odone; int eoi; double oi_ratio;} io_t;
#define SRC_DATA io_t
typedef struct  soxr SRC_STATE;
#define src_callback_t soxr_input_fn_t
#define SRC_ERROR soxr_error_t
#define SRC_SRCTYPE unsigned

#include "soxr-lsr.h"
#include "rint.h"

soxr_error_t src_simple(io_t * p, unsigned id, int channels)
{
  size_t idone, odone;
  soxr_error_t error;
  soxr_quality_spec_t q_spec = soxr_quality_spec(SOXR_LSR0Q + id, 0);
  char const * e = getenv("SOXR_LSR_NUM_THREADS");
  soxr_runtime_spec_t r_spec = soxr_runtime_spec(!(e && atoi(e) != 1));
  assert (channels > 0);
  assert (p->ilen >= 0);
  assert (p->olen >= 0);
  error = soxr_oneshot(1, p->oi_ratio, (unsigned)channels,
      p->in, (size_t)p->ilen, &idone, p->out, (size_t)p->olen, &odone,
      0, &q_spec, &r_spec);
  p->idone = (long)idone, p->odone = (long)odone;
  return error;
}

soxr_t src_callback_new(soxr_input_fn_t fn, unsigned id, int channels, SRC_ERROR * error0, void * p)
{
  soxr_quality_spec_t q_spec = soxr_quality_spec(SOXR_LSR0Q + id, 0);
  char const * e = getenv("SOXR_LSR_NUM_THREADS");
  soxr_runtime_spec_t r_spec = soxr_runtime_spec(!(e && atoi(e) != 1));
  soxr_error_t error;
  soxr_t soxr = 0;
  assert (channels > 0);
  /* To minimise latency e.g. for real-time playback:
  if (id == 2)
    r_spec.log2_large_dft_size = r_spec.log2_min_dft_size = 8;
    */
  soxr = soxr_create(0, 0, (unsigned)channels, &error, 0, &q_spec, &r_spec);
  if (soxr)
    error = soxr_set_input_fn(soxr, fn, p, 0);
  if (error0)
    *(int *)error0 = (int)(ptrdiff_t)error;
  return soxr;
}

soxr_error_t src_process(soxr_t p, io_t * io)
{
  if (!p || !io) return "null pointer";
  soxr_set_error(p, soxr_set_io_ratio(p, 1/io->oi_ratio, (size_t)io->olen));

  { size_t idone , odone;
  soxr_process(p, io->in, (size_t)(io->eoi? ~io->ilen : io->ilen), /* hack */
      &idone, io->out, (size_t)io->olen, &odone);
  io->idone = (long)idone, io->odone = (long)odone;
  return soxr_error(p); }
}

long src_callback_read(soxr_t p, double oi_ratio, long olen, float * obuf)
{
  if (!p || olen < 0) return -1;
  soxr_set_error(p, soxr_set_io_ratio(p, 1/oi_ratio, (size_t)olen));
  return (long)soxr_output(p, obuf, (size_t)olen);
}

void src_float_to_short_array(float const * src, short * dest, int len)
{
  double d, N = 1. + SHRT_MAX;
  assert (src && dest);
  while (len--) d = src[len] * N, dest[len] = (short)(d > N - 1? (short)(N - 1) : d < -N? (short)-N : rint16(d));
}

void src_short_to_float_array(short const * src, float * dest, int len)
{
  assert (src && dest);
  while (len--) dest[len] = (float)(src[len] * (1 / (1. + SHRT_MAX)));
}

void src_float_to_int_array(float const * src, int * dest, int len)
{
  double d, N = 32768. * 65536.; /* N.B. int32, not int! (Also next fn.) */
  assert (src && dest);
  while (len--) d = src[len] * N, dest[len] = d >= N - 1? (int)(N - 1) : d < -N? (int)(-N) : rint32(d);
}

void src_int_to_float_array(int const * src, float * dest, int len)
{
  assert (src && dest);
  while (len--) dest[len] = (float)(src[len] * (1 / (32768. * 65536.)));
}

static char const * const names[] = {"LSR best sinc", "LSR medium sinc", "LSR fastest sinc", "LSR ZOH", "LSR linear", "SoX VHQ"};
char const * src_get_name(unsigned n)         {return n < 5u + !getenv("SOXR_LSR_STRICT")? names[n] : 0;}
char const * src_get_description(unsigned id) {return src_get_name(id);}
char const * src_get_version(void)            {return soxr_version();}
char const * src_strerror(soxr_error_t error) {return error == (soxr_error_t)1? "Placeholder." : sizeof(int) >= sizeof(char *) || !error ? soxr_strerror(error) : "soxr error";}
int src_is_valid_ratio(double oi_ratio)       {return getenv("SOXR_LSR_STRICT")? oi_ratio >= 1./256 && oi_ratio <= 256 : oi_ratio > 0;}
soxr_error_t src_error(soxr_t p)              {return soxr_error(p);}
soxr_error_t src_reset(soxr_t p)              {return soxr_clear(p);}
soxr_t src_delete(soxr_t p)                   {soxr_delete(p); return 0;}
soxr_error_t src_set_ratio(soxr_t p, double oi_ratio) {return soxr_set_io_ratio(p, 1/oi_ratio, 0);}
soxr_t src_new(unsigned id, int channels, SRC_ERROR * error) {return src_callback_new(0, id, channels, error, 0);}
