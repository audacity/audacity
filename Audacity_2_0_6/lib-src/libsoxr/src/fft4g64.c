/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include "filter.h"
#include "fft4g.c"
#include "soxr-config.h"

#if HAVE_DOUBLE_PRECISION
static void * null(void) {return 0;}
static void nothing(void) {}
static void forward (int length, void * setup, double * H) {lsx_safe_rdft(length,  1, H); (void)setup;}
static void backward(int length, void * setup, double * H) {lsx_safe_rdft(length, -1, H); (void)setup;}
static int multiplier(void) {return 2;}

typedef void (* fn_t)(void);
fn_t _soxr_rdft64_cb[] = {
  (fn_t)null,
  (fn_t)null,
  (fn_t)nothing,
  (fn_t)forward,
  (fn_t)forward,
  (fn_t)backward,
  (fn_t)backward,
  (fn_t)_soxr_ordered_convolve,
  (fn_t)_soxr_ordered_partial_convolve,
  (fn_t)multiplier,
  (fn_t)nothing,
};
#endif
