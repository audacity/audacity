/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined PFFT_MACROS_ONLY

#include "math-wrap.h"

#if PFFFT_DOUBLE
  #include "util64s.h"
#else
  #include "util32s.h"
  #define sin(x) sinf(x)
  #define cos(x) cosf(x)
#endif

#define pffft_aligned_free    SIMD_ALIGNED_FREE
#define pffft_aligned_malloc  SIMD_ALIGNED_MALLOC
#define pffft_aligned_calloc  SIMD_ALIGNED_CALLOC

#undef inline
#define inline __inline

#endif



#include "pffft.c"



#if !defined PFFT_MACROS_ONLY

#if !defined PFFFT_SIMD_DISABLE

static void pffft_zconvolve(PFFFT_Setup *s, const float *a, const float *b, float *ab) {
  int i, Ncvec = s->Ncvec;
  const v4sf * /*RESTRICT*/ va = (const v4sf*)a;
  const v4sf * RESTRICT vb = (const v4sf*)b;
  v4sf * /*RESTRICT*/ vab = (v4sf*)ab;

  float ar, ai, br, bi;

#if defined(__arm__) || defined (_M_ARM64)
  __builtin_prefetch(va);
  __builtin_prefetch(vb);
  __builtin_prefetch(va+2);
  __builtin_prefetch(vb+2);
  __builtin_prefetch(va+4);
  __builtin_prefetch(vb+4);
  __builtin_prefetch(va+6);
  __builtin_prefetch(vb+6);
#endif

  assert(VALIGNED(a) && VALIGNED(b) && VALIGNED(ab));
  ar = ((v4sf_union*)va)[0].f[0];
  ai = ((v4sf_union*)va)[1].f[0];
  br = ((v4sf_union*)vb)[0].f[0];
  bi = ((v4sf_union*)vb)[1].f[0];

  for (i=0; i < Ncvec; i += 2) {
    v4sf ar, ai, br, bi;
    ar = va[2*i+0]; ai = va[2*i+1];
    br = vb[2*i+0]; bi = vb[2*i+1];
    VCPLXMUL(ar, ai, br, bi);
    vab[2*i+0] = ar;
    vab[2*i+1] = ai;
    ar = va[2*i+2]; ai = va[2*i+3];
    br = vb[2*i+2]; bi = vb[2*i+3];
    VCPLXMUL(ar, ai, br, bi);
    vab[2*i+2] = ar;
    vab[2*i+3] = ai;
  }
  if (s->transform == PFFFT_REAL) {
    ((v4sf_union*)vab)[0].f[0] = ar*br;
    ((v4sf_union*)vab)[1].f[0] = ai*bi;
  }
}

#else

static void pffft_zconvolve(PFFFT_Setup *s, const float *a, const float *b, float *ab) {
  int i, Ncvec = s->Ncvec;

  if (s->transform == PFFFT_REAL) {
    /* take care of the fftpack ordering */
    ab[0] = a[0]*b[0];
    ab[2*Ncvec-1] = a[2*Ncvec-1]*b[2*Ncvec-1];
    ++ab; ++a; ++b; --Ncvec;
  }
  for (i=0; i < Ncvec; ++i) {
    float ar, ai, br, bi;
    ar = a[2*i+0]; ai = a[2*i+1];
    br = b[2*i+0]; bi = b[2*i+1];
    VCPLXMUL(ar, ai, br, bi);
    ab[2*i+0] = ar;
    ab[2*i+1] = ai;
  }
}

#endif

#include <string.h>

static void pffft_reorder_back(int length, void * setup, float * data, float * work)
{
  memcpy(work, data, (unsigned)length * sizeof(*work));
  pffft_zreorder(setup, work, data, PFFFT_BACKWARD);
}

#endif
