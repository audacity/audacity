/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include <limits.h>
#include <math.h>
#include <string.h>

#include "data-io.h"
#include "internal.h"



#define DEINTERLEAVE_FROM(T,flag) do { \
  unsigned i; \
  size_t j; \
  T const * src = *src0; \
  if (ch > 1) for (j = 0; j < n; ++j) \
    for (i = 0; i < ch; ++i) dest[i][j] = (DEINTERLEAVE_TO)*src++; \
  else if (flag) memcpy(dest[0], src, n * sizeof(T)), src = &src[n]; \
  else for (j = 0; j < n; dest[0][j++] = (DEINTERLEAVE_TO)*src++); \
  *src0 = src; \
} while (0)



#if WITH_CR64 || WITH_CR64S
void _soxr_deinterleave(double * * dest, /* Round/clipping not needed here */
    soxr_datatype_t data_type, void const * * src0, size_t n, unsigned ch)
{
#define DEINTERLEAVE_TO double
  switch (data_type & 3) {
    case SOXR_FLOAT32: DEINTERLEAVE_FROM(float, 0); break;
    case SOXR_FLOAT64: DEINTERLEAVE_FROM(double, 1); break;
    case SOXR_INT32:   DEINTERLEAVE_FROM(int32_t, 0); break;
    case SOXR_INT16:   DEINTERLEAVE_FROM(int16_t, 0); break;
    default: break;
  }
}
#endif



#if WITH_CR32 || WITH_CR32S || WITH_VR32
void _soxr_deinterleave_f(float * * dest, /* Round/clipping not needed here */
    soxr_datatype_t data_type, void const * * src0, size_t n, unsigned ch)
{
#undef DEINTERLEAVE_TO
#define DEINTERLEAVE_TO float
  switch (data_type & 3) {
    case SOXR_FLOAT32: DEINTERLEAVE_FROM(float, 1); break;
    case SOXR_FLOAT64: DEINTERLEAVE_FROM(double, 0); break;
    case SOXR_INT32:   DEINTERLEAVE_FROM(int32_t, 0); break;
    case SOXR_INT16:   DEINTERLEAVE_FROM(int16_t, 0); break;
    default: break;
  }
}
#endif



#include "rint.h"



#if defined FE_INVALID && defined FPU_RINT32 && defined __STDC_VERSION__
  #if __STDC_VERSION__ >= 199901L
    #pragma STDC FENV_ACCESS ON
  #endif
#endif

#if WITH_CR64 || WITH_CR64S
#define FLOATX double

#define LSX_RINT_CLIP_2 lsx_rint32_clip_2
#define LSX_RINT_CLIP lsx_rint32_clip
#define RINT_CLIP rint32_clip
#define RINT rint32D
#if defined FPU_RINT32
  #define FPU_RINT
#endif
#define RINT_T int32_t
#define RINT_MAX 2147483647L
#include "rint-clip.h"

#define LSX_RINT_CLIP_2 lsx_rint16_clip_2
#define LSX_RINT_CLIP lsx_rint16_clip
#define RINT_CLIP rint16_clip
#define RINT rint16D
#if defined FPU_RINT16
  #define FPU_RINT
#endif
#define RINT_T int16_t
#define RINT_MAX 32767
#include "rint-clip.h"

#define LSX_RINT_CLIP_2 lsx_rint16_clip_2_dither
#define LSX_RINT_CLIP lsx_rint16_clip_dither
#define RINT_CLIP rint16_clip_dither
#define RINT rint16D
#if defined FPU_RINT16
  #define FPU_RINT
#endif
#define RINT_T int16_t
#define RINT_MAX 32767
#define DITHER
#include "rint-clip.h"

#undef FLOATX
#endif



#if WITH_CR32 || WITH_CR32S || WITH_VR32
#define FLOATX float

#define LSX_RINT_CLIP_2 lsx_rint32_clip_2_f
#define LSX_RINT_CLIP lsx_rint32_clip_f
#define RINT_CLIP rint32_clip_f
#define RINT rint32F
#if defined FPU_RINT32
  #define FPU_RINT
#endif
#define RINT_T int32_t
#define RINT_MAX 2147483647L
#include "rint-clip.h"

#define LSX_RINT_CLIP_2 lsx_rint16_clip_2_f
#define LSX_RINT_CLIP lsx_rint16_clip_f
#define RINT_CLIP rint16_clip_f
#define RINT rint16F
#if defined FPU_RINT16
  #define FPU_RINT
#endif
#define RINT_T int16_t
#define RINT_MAX 32767
#include "rint-clip.h"

#define LSX_RINT_CLIP_2 lsx_rint16_clip_2_dither_f
#define LSX_RINT_CLIP lsx_rint16_clip_dither_f
#define RINT_CLIP rint16_clip_dither_f
#define RINT rint16D
#if defined FPU_RINT16
  #define FPU_RINT
#endif
#define RINT_T int16_t
#define RINT_MAX 32767
#define DITHER
#include "rint-clip.h"

#undef FLOATX
#endif

#if defined FE_INVALID && defined FPU_RINT32 && defined __STDC_VERSION__
  #if __STDC_VERSION__ >= 199901L
    #pragma STDC FENV_ACCESS OFF
  #endif
#endif



#define INTERLEAVE_TO(T,flag) do { \
  unsigned i; \
  size_t j; \
  T * dest = *dest0; \
  if (ch > 1) \
  for (j = 0; j < n; ++j) for (i = 0; i < ch; ++i) *dest++ = (T)src[i][j]; \
  else if (flag) memcpy(dest, src[0], n * sizeof(T)), dest = &dest[n]; \
  else for (j = 0; j < n; *dest++ = (T)src[0][j++]); \
  *dest0 = dest; \
  return 0; \
} while (0)

#if WITH_CR64 || WITH_CR64S
size_t /* clips */ _soxr_interleave(soxr_datatype_t data_type, void * * dest0,
  double const * const * src, size_t n, unsigned ch, unsigned long * seed)
{
  switch (data_type & 3) {
    case SOXR_FLOAT32: INTERLEAVE_TO(float, 0);
    case SOXR_FLOAT64: INTERLEAVE_TO(double, 1);

    case SOXR_INT32: if (ch == 1)
        return lsx_rint32_clip(dest0, src[0], n);
      return lsx_rint32_clip_2(dest0, src, ch, n);

    case SOXR_INT16: if (seed) {
      if (ch == 1)
        return lsx_rint16_clip_dither(dest0, src[0], n, seed);
      return lsx_rint16_clip_2_dither(dest0, src, ch, n, seed);
    }
    if (ch == 1)
        return lsx_rint16_clip(dest0, src[0], n);
      return lsx_rint16_clip_2(dest0, src, ch, n);
    default: break;
  }
  return 0;
}
#endif

#if WITH_CR32 || WITH_CR32S || WITH_VR32
size_t /* clips */ _soxr_interleave_f(soxr_datatype_t data_type, void * * dest0,
  float const * const * src, size_t n, unsigned ch, unsigned long * seed)
{
  switch (data_type & 3) {
    case SOXR_FLOAT32: INTERLEAVE_TO(float, 1);
    case SOXR_FLOAT64: INTERLEAVE_TO(double, 0);

    case SOXR_INT32: if (ch == 1)
        return lsx_rint32_clip_f(dest0, src[0], n);
      return lsx_rint32_clip_2_f(dest0, src, ch, n);

    case SOXR_INT16: if (seed) {
      if (ch == 1)
        return lsx_rint16_clip_dither_f(dest0, src[0], n, seed);
      return lsx_rint16_clip_2_dither_f(dest0, src, ch, n, seed);
    }
    if (ch == 1)
        return lsx_rint16_clip_f(dest0, src[0], n);
      return lsx_rint16_clip_2_f(dest0, src, ch, n);
    default: break;
  }
  return 0;
}
#endif
