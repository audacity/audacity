/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "simd.h"
#include "simd-dev.h"

#define SIMD_ALIGNMENT (sizeof(float) * 4)

void * _soxr_simd_aligned_malloc(size_t size)
{
  char * p1 = 0, * p = malloc(size + SIMD_ALIGNMENT);
  if (p) {
    p1 = (char *)((size_t)(p + SIMD_ALIGNMENT) & ~(SIMD_ALIGNMENT - 1));
    *((void * *)p1 - 1) = p;
  }
  return p1;
}



void * _soxr_simd_aligned_calloc(size_t nmemb, size_t size)
{
  void * p = _soxr_simd_aligned_malloc(nmemb * size);
  if (p)
    memset(p, 0, nmemb * size);
  return p;
}



void _soxr_simd_aligned_free(void * p1)
{
  if (p1)
    free(*((void * *)p1 - 1));
}



void _soxr_ordered_convolve_simd(int n, void * not_used, float * a, const float * b)
{
  int i;
  float ab0, ab1;
  v4sf       * /*RESTRICT*/ va = (v4sf       *)a;
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



void _soxr_ordered_partial_convolve_simd(int n, float * a, const float * b)
{
  int i;
  float ab0;
  v4sf       * /*RESTRICT*/ va = (v4sf       *)a;
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
