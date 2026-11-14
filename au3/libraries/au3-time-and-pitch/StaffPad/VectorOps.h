//
// This header is provided for convenience, to easily wrap vector operations around
// their platform-specific optimised libraries (e.g. IPP, vDSP), if desired.
// This can be done by adding #if defined(...) compile-time branches to each function
// and calling the corresponding library function, e.g. ippsAdd_32f or vDSP_vadd
// inside add().
//

#pragma once

#include <stdlib.h>

#include <complex>
#include <cstdint>
#include <cstring>

#if defined(_MSC_VER) && (defined(_M_AMD64) || defined(_M_X64) || \
    (defined(_M_IX86_FP) && _M_IX86_FP >= 2))
#define USE_SSE2_COMPLEX 1
#endif

#if USE_SSE2_COMPLEX
#   include "SimdComplexConversions_sse2.h"
#endif

namespace staffpad {
namespace vo {
inline void* allocate(int32_t bytes)
{
    return ::malloc(bytes);
}

inline void free(void* ptr)
{
    return ::free(ptr);
}

template<class T>
inline void copy(const T* src, T* dst, int32_t n)
{
    memcpy(dst, src, n * sizeof(T));
}

template<class T>
inline void add(const T* src1, const T* src2, T* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = src1[i] + src2[i];
    }
}

template<class T>
inline void subtract(const T* src1, const T* src2, T* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = src2[i] - src1[i];
    }
}

template<class T>
inline void constantMultiply(const T* src, T constant, T* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = src[i] * constant;
    }
}

template<class T>
inline void constantMultiplyAndAdd(const T* src, T constant, T* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] += src[i] * constant;
    }
}

template<class T>
inline void multiply(const T* src1, const T* src2, T* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = src1[i] * src2[i];
    }
}

template<class T>
inline void setToZero(T* dst, int32_t n)
{
    std::fill(dst, dst + n, 0.f);
}

template<class T>
inline void findMaxElement(const T* src, int32_t n, int32_t& maxIndex, T& maxValue)
{
    maxIndex = 0;
    maxValue = n > 0 ? src[0] : std::numeric_limits<T>::min();

    for (int32_t i = 1; i < n; i++) {
        if (src[i] > maxValue) {
            maxValue = src[i];
            maxIndex = i;
        }
    }
}

#if USE_SSE2_COMPLEX

inline void calcPhases(const std::complex<float>* src, float* dst, int32_t n)
{
    simd_complex_conversions::perform_parallel_simd_aligned(
        src, dst, n,
        [](const __m128 rp, const __m128 ip, __m128& out)
    { out = simd_complex_conversions::atan2_ps(ip, rp); });
}

inline void calcNorms(const std::complex<float>* src, float* dst, int32_t n)
{
    simd_complex_conversions::perform_parallel_simd_aligned(
        src, dst, n,
        [](const __m128 rp, const __m128 ip, __m128& out)
    { out = simd_complex_conversions::norm(rp, ip); });
}

inline void rotate(
    const float* oldPhase, const float* newPhase, std::complex<float>* dst,
    int32_t n)
{
    simd_complex_conversions::rotate_parallel_simd_aligned(
        oldPhase, newPhase, dst, n);
}

#else
inline void calcPhases(const std::complex<float>* src, float* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = std::arg(src[i]);
    }
}

inline void calcNorms(const std::complex<float>* src, float* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        dst[i] = std::norm(src[i]);
    }
}

inline void rotate(const float* oldPhase, const float* newPhase, std::complex<float>* dst, int32_t n)
{
    for (int32_t i = 0; i < n; i++) {
        const auto theta = oldPhase ? newPhase[i] - oldPhase[i] : newPhase[i];
        dst[i] *= std::complex<float>(cosf(theta), sinf(theta));
    }
}

#endif
} // namespace vo
} // namespace staffpad
