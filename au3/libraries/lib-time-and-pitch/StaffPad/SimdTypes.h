/*
  Simd-types for parallel dsp processing.
  Aligned memory allocation for simd vectors.
 */

#pragma once
#include <cassert>
#include <cstdlib>

#if _MSC_VER
#define __finl __forceinline
#define __vecc __vectorcall
#else
#define __finl inline __attribute__((always_inline))
#define __vecc
#endif

#if defined(__SSE2__) || (defined(_M_AMD64) || defined(_M_X64)) || (defined(_M_IX86_FP) && _M_IX86_FP >= 2)
#include "SimdTypes_sse2.h"
#elif defined(__arm64__) || defined(__aarch64__) || defined(_M_ARM64)
#include "SimdTypes_neon.h"
#else
#include "SimdTypes_scalar.h"
#endif

namespace staffpad::audio::simd {
/// reserve aligned memory. Needs to be freed with aligned_free()
inline void* aligned_malloc(size_t required_bytes, size_t alignment)
{
    auto offset = alignment - 1 + sizeof(void*);
    auto p1 = std::malloc(required_bytes + offset);
    if (p1 == nullptr) {
        return nullptr;
    }
    // figure out aligned position
    void* p2 = (void*)(((size_t)(p1) + offset) & ~(alignment - 1));
    // write malloced pointer in front of aligned data
    ((void**)p2)[-1] = p1;
    return p2;
}

/// free memory allocated with aligned_malloc
inline void aligned_free(void* p)
{
    if (p) {
        free(((void**)p)[-1]);
    }
}

/// create a c++ class at an memory-aligned spot that needs to be deleted using aligned_delete
template<typename cls>
inline cls* aligned_new(int alignment)
{
    void* mem = aligned_malloc(sizeof(cls), alignment);
    return new (mem) cls();
}

/** delete objects created using aligned_new */
template<typename cls>
inline void aligned_delete(cls* obj)
{
    if (obj != nullptr) {
        obj->~cls();
        aligned_free((void*)obj);
    }
}

template<typename T>
inline bool is_aligned(T* obj, int alignment)
{
    return (((size_t)obj) & (alignment - 1)) == 0;
}

/** this template allows to write float SIMD code with the supported operators the following way:

float *a_vec;
const float *b_vec;
perform_parallel_simd_aligned(a_vec, b_vec, 512, [](auto &a, auto &b) {
auto t = a;
a = 3.f * a + 12.f * b;
b = 0.25f * a + 3.f * b;
});
*/

// two buffers read/write
template<typename fnc>
__finl void perform_parallel_simd_aligned(float* a, float* b, int n, const fnc& f)
{
    // fnc& f needs to be a lambda of type [](auto &a, auto &b){}.
    // the autos will be float_x4/float
    constexpr int N = 4;
    constexpr int byte_size = sizeof(float);

    assert(is_aligned(a, N * byte_size) && is_aligned(b, N * byte_size));

    for (int i = 0; i <= n - N; i += N) {
        auto x = float_x4_load_aligned(a + i);
        auto y = float_x4_load_aligned(b + i);
        f(x, y);
        store_aligned(x, a + i);
        store_aligned(y, b + i);
    }
    // deal with last partial packet
    for (int i = n & (~(N - 1)); i < n; ++i) {
        f(a[i], b[i]);
    }
}

/// template for applying math to one data buffer
template<typename fnc>
__finl void perform_parallel_simd_aligned(float* a, int n, const fnc& f)
{
    // fnc& f needs to be a lambda of type [](auto &a){}.
    constexpr int N = 4;
    constexpr int byte_size = sizeof(float);
    assert(is_aligned(a, N * byte_size));

    for (int i = 0; i <= n - N; i += N) {
        auto x = float_x4_load_aligned(a + i);
        f(x);
        store_aligned(x, a + i);
    }
    // deal with last partial packet
    for (int i = n & (~(N - 1)); i < n; ++i) {
        f(a[i]);
    }
}
} // namespace staffpad::audio::simd
