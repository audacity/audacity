/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if defined DITHER

#define DITHERING + (1./32)*(int)(((ran1>>=3)&31)-((ran2>>=3)&31))
#define DITHER_RAND (seed = 1664525UL * seed + 1013904223UL) >> 3
#define DITHER_VARS unsigned long ran1 = DITHER_RAND, ran2 = DITHER_RAND
#define SEED_ARG , unsigned long * seed0
#define SAVE_SEED *seed0 = seed
#define COPY_SEED unsigned long seed = *seed0;
#define COPY_SEED1 unsigned long seed1 = seed
#define PASS_SEED1 , &seed1
#define PASS_SEED  , &seed
#define FLOATD double

#else

#define DITHERING
#define DITHER_VARS
#define SEED_ARG
#define SAVE_SEED
#define COPY_SEED
#define COPY_SEED1
#define PASS_SEED1
#define PASS_SEED
#define FLOATD FLOATX

#endif

#define DO_16 _;_;_;_;_;_;_;_;_;_;_;_;_;_;_;_



#if defined FE_INVALID && defined FPU_RINT
static void RINT_CLIP(RINT_T * const dest, FLOATX const * const src,
    unsigned stride, size_t i, size_t const n, size_t * const clips SEED_ARG)
{
  COPY_SEED
  DITHER_VARS;
  for (; i < n; ++i) {
    FLOATD const d = src[i] DITHERING;
    RINT(dest[stride * i], d);
    if (fe_test_invalid()) {
      fe_clear_invalid();
      dest[stride * i] = d > 0? RINT_MAX : -RINT_MAX - 1;
      ++*clips;
    }
  }
  SAVE_SEED;
}
#endif



static size_t LSX_RINT_CLIP(void * * const dest0, FLOATX const * const src,
    size_t const n SEED_ARG)
{
  size_t i, clips = 0;
  RINT_T * dest = *dest0;
  COPY_SEED
#if defined FE_INVALID && defined FPU_RINT
#define _ RINT(dest[i], src[i] DITHERING); ++i
  for (i = 0; i < (n & ~15u);) {
    COPY_SEED1;
    DITHER_VARS;
    DO_16;
    if (fe_test_invalid()) {
      fe_clear_invalid();
      RINT_CLIP(dest, src, 1, i - 16, i, &clips PASS_SEED1);
    }
  }
  RINT_CLIP(dest, src, 1, i, n, &clips PASS_SEED);
#else
#define _ d = src[i] DITHERING, dest[i++] = (RINT_T)(d > 0? \
    d+.5 >= N? ++clips, N-1 : d+.5 : d-.5 <= -N-1? ++clips, -N:d-.5)
  const double N = 1. + RINT_MAX;
  double d;
  for (i = 0; i < (n & ~15u);) {
    DITHER_VARS;
    DO_16;
  }
  {
    DITHER_VARS;
    for (; i < n; _);
  }
#endif
  SAVE_SEED;
  *dest0 = dest + n;
  return clips;
}
#undef _



static size_t LSX_RINT_CLIP_2(void * * dest0, FLOATX const * const * srcs,
    unsigned const stride, size_t const n SEED_ARG)
{
  unsigned j;
  size_t i, clips = 0;
  RINT_T * dest = *dest0;
  COPY_SEED
#if defined FE_INVALID && defined FPU_RINT
#define _ RINT(dest[stride * i], src[i] DITHERING); ++i
  for (j = 0; j < stride; ++j, ++dest) {
    FLOATX const * const src = srcs[j];
    for (i = 0; i < (n & ~15u);) {
      COPY_SEED1;
      DITHER_VARS;
      DO_16;
      if (fe_test_invalid()) {
        fe_clear_invalid();
        RINT_CLIP(dest, src, stride, i - 16, i, &clips PASS_SEED1);
      }
    }
    RINT_CLIP(dest, src, stride, i, n, &clips PASS_SEED);
  }
#else
#define _ d = src[i] DITHERING, dest[stride * i++] = (RINT_T)(d > 0? \
    d+.5 >= N? ++clips, N-1 : d+.5 : d-.5 <= -N-1? ++clips, -N:d-.5)
  const double N = 1. + RINT_MAX;
  double d;
  for (j = 0; j < stride; ++j, ++dest) {
    FLOATX const * const src = srcs[j];
    for (i = 0; i < (n & ~15u);) {
      DITHER_VARS;
      DO_16;
    }
    {
      DITHER_VARS;
      for (; i < n; _);
    }
  }
#endif
  SAVE_SEED;
  *dest0 = dest + stride * (n - 1);
  return clips;
}
#undef _

#undef FLOATD
#undef PASS_SEED
#undef PASS_SEED1
#undef COPY_SEED1
#undef COPY_SEED
#undef SAVE_SEED
#undef SEED_ARG
#undef DITHER_VARS
#undef DITHERING
#undef DITHER

#undef RINT_MAX
#undef RINT_T
#undef FPU_RINT
#undef RINT
#undef RINT_CLIP
#undef LSX_RINT_CLIP
#undef LSX_RINT_CLIP_2
