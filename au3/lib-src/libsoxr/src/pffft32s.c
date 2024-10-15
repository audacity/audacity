/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#define PFFFT_DOUBLE 0
#include "pffft-wrap.c"

#include "rdft_t.h"

static void * setup(int len) {return pffft_new_setup(len, PFFFT_REAL);}
static void forward  (int length, void * setup, float * h, float * scratch) {pffft_transform        (setup, h, h, scratch, PFFFT_FORWARD); (void)length;}
static void oforward (int length, void * setup, float * h, float * scratch) {pffft_transform_ordered(setup, h, h, scratch, PFFFT_FORWARD); (void)length;}
static void backward (int length, void * setup, float * H, float * scratch) {pffft_transform        (setup, H, H, scratch, PFFFT_BACKWARD);(void)length;}
static void obackward(int length, void * setup, float * H, float * scratch) {pffft_transform_ordered(setup, H, H, scratch, PFFFT_BACKWARD);(void)length;}
static void convolve(int length, void * setup, float * H, float const * with) {pffft_zconvolve(setup, H, with, H); (void)length;}
static int multiplier(void) {return 1;}
static int flags(void) {return RDFT_IS_SIMD | RDFT_NEEDS_SCRATCH;}

fn_t _soxr_rdft32s_cb[] = {
  (fn_t)setup,
  (fn_t)setup,
  (fn_t)pffft_destroy_setup,
  (fn_t)forward,
  (fn_t)oforward,
  (fn_t)backward,
  (fn_t)obackward,
  (fn_t)convolve,
  (fn_t)ORDERED_PARTIAL_CONVOLVE_SIMD,
  (fn_t)multiplier,
  (fn_t)pffft_reorder_back,
  (fn_t)SIMD_ALIGNED_MALLOC,
  (fn_t)SIMD_ALIGNED_CALLOC,
  (fn_t)SIMD_ALIGNED_FREE,
  (fn_t)flags,
};
