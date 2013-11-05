/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

#if !defined simd_included
#define simd_included

#include <stddef.h>

void * _soxr_simd_aligned_malloc(size_t);
void * _soxr_simd_aligned_calloc(size_t, size_t);
void _soxr_simd_aligned_free(void *);

void _soxr_ordered_convolve_simd(int n, void * not_used, float * a, const float * b);
void _soxr_ordered_partial_convolve_simd(int n, float * a, const float * b);

#endif
