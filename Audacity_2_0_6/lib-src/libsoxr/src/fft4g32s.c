/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include "filter.h"
#include "simd.h"

static void * null(void) {return 0;}
static void nothing(void) {}
static void forward (int length, void * setup, float * H) {lsx_safe_rdft_f(length,  1, H); (void)setup;}
static void backward(int length, void * setup, float * H) {lsx_safe_rdft_f(length, -1, H); (void)setup;}
static int multiplier(void) {return 2;}

typedef void (* fn_t)(void);
fn_t _soxr_rdft32s_cb[] = {
  (fn_t)null,
  (fn_t)null,
  (fn_t)nothing,
  (fn_t)forward,
  (fn_t)forward,
  (fn_t)backward,
  (fn_t)backward,
  (fn_t)_soxr_ordered_convolve_simd,
  (fn_t)_soxr_ordered_partial_convolve_simd,
  (fn_t)multiplier,
  (fn_t)nothing,
};
