/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#define RATE_CB    _soxr_rate32s_cb
#define CORE_STR   "cr32s"

#define CORE_TYPE  (CORE_SIMD_POLY|CORE_SIMD_HALF|CORE_SIMD_DFT)
#include "cr-core.c"
