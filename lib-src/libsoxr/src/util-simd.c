/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "soxr-config.h"

#define SIMD_ALIGNMENT (sizeof(float) * (1 + (PFFFT_DOUBLE|AVCODEC_FOUND)) * 4)

void * SIMD_ALIGNED_MALLOC(size_t size)
{
  char * p1 = 0, * p = malloc(size + SIMD_ALIGNMENT);
  if (p) {
    p1 = (char *)((size_t)(p + SIMD_ALIGNMENT) & ~(SIMD_ALIGNMENT - 1));
    *((void * *)p1 - 1) = p;
  }
  return p1;
}



void * SIMD_ALIGNED_CALLOC(size_t nmemb, size_t size)
{
  void * p = SIMD_ALIGNED_MALLOC(nmemb * size);
  if (p)
    memset(p, 0, nmemb * size);
  return p;
}



void SIMD_ALIGNED_FREE(void * p1)
{
  if (p1)
    free(*((void * *)p1 - 1));
}



#define PFFT_MACROS_ONLY
#include "pffft.c"



void ORDERED_CONVOLVE_SIMD(int n, void * not_used, float * a, float const * b)
{
  int i;
  float ab0, ab1;
  v4sf       *   RESTRICT   va = (v4sf       *)a;
  v4sf const *   RESTRICT   vb = (v4sf const *)b;
  assert(VALIGNED(a) && VALIGNED(b));
  ab0 = a[0] * b[0], ab1 = a[1] * b[1];
  for (i = 0; i < n / 4; i += 2) {
    v4sf a1r = va[i+0], a1i = va[i+1];
    v4sf b1r = vb[i+0], b1i = vb[i+1];
    UNINTERLEAVE2(a1r, a1i, a1r, a1i);
    UNINTERLEAVE2(b1r, b1i, b1r, b1i);
    VCPLXMUL(a1r, a1i, b1r, b1i);
    INTERLEAVE2(a1r, a1i, a1r, a1i);
    va[i+0] = a1r, va[i+1] = a1i;
  }
  a[0] = ab0, a[1] = ab1;
  (void)not_used;
}



void ORDERED_PARTIAL_CONVOLVE_SIMD(int n, float * a, float const * b)
{
  int i;
  float ab0;
  v4sf       *   RESTRICT   va = (v4sf       *)a;
  v4sf const *   RESTRICT   vb = (v4sf const *)b;
  assert(VALIGNED(a) && VALIGNED(b));
  ab0 = a[0] * b[0];
  for (i = 0; i < n / 4; i += 2) {
    v4sf a1r = va[i+0], a1i = va[i+1];
    v4sf b1r = vb[i+0], b1i = vb[i+1];
    UNINTERLEAVE2(a1r, a1i, a1r, a1i);
    UNINTERLEAVE2(b1r, b1i, b1r, b1i);
    VCPLXMUL(a1r, a1i, b1r, b1i);
    INTERLEAVE2(a1r, a1i, a1r, a1i);
    va[i+0] = a1r, va[i+1] = a1i;
  }
  a[0] = ab0;
  a[1] = b[n] * a[n] - b[n+1] * a[n+1];
}
