/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#define sample_t   float
#define RATE_SIMD  0
#define RDFT_CB    _soxr_rdft32_cb
#define RATE_CB    _soxr_rate32_cb
#define RATE_ID    "single-precision"
#include "rate.h"
