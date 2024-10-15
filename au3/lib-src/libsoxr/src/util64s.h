/* SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined soxr_util64s_included
#define soxr_util64s_included

#include <stddef.h>

void * _soxr_simd64_aligned_malloc(size_t);
void * _soxr_simd64_aligned_calloc(size_t, size_t);
void _soxr_simd64_aligned_free(void *);

#define SIMD_ALIGNED_MALLOC _soxr_simd64_aligned_malloc
#define SIMD_ALIGNED_CALLOC _soxr_simd64_aligned_calloc
#define SIMD_ALIGNED_FREE _soxr_simd64_aligned_free

void _soxr_ordered_convolve_simd64(int n, void * not_used, double * a, double const * b);
void _soxr_ordered_partial_convolve_simd64(int n, double * a, double const * b);

#define ORDERED_CONVOLVE_SIMD _soxr_ordered_convolve_simd64
#define ORDERED_PARTIAL_CONVOLVE_SIMD _soxr_ordered_partial_convolve_simd64

#endif
