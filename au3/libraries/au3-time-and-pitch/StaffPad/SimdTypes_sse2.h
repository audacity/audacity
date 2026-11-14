/*
SSE2 simd types
*/

#pragma once

#if _MSC_VER
#define __finl __forceinline
#define __vecc __vectorcall
#else
#define __finl inline __attribute__((always_inline))
#define __vecc
#endif

#include <emmintrin.h>

namespace staffpad::audio::simd {
// this is jumping through some hoops to get the same level of support
// for clang and msvc. With clang, the sse2 types are built-in and have
// some arithmetic operators defined.
// On msvc the sse2 types are structs with no operators.
// to get to the same level and to be able to write algorithms "naturally",
// everything needs to be wrapped in a struct.
struct float_x4
{
    __m128 s;
    __finl float_x4()
    {
    }

    /// enables math like: float_x4 a = 0.5f * float_x4{1.f, 2.f, 3.f, 4.f};
    __finl float_x4(float val)
    {
        s = _mm_set1_ps(val);
    }

    __finl float_x4(const __m128& val)
        : s(val)
    {
    }

#if __clang__
private:
    // this helper class allows writing to the single registers for clang
    // __mm128 is a built-in type -> we can't return a float& reference.
    // this is just syntax sugar and clang will remove it during builds.
    //
    // it allows to write
    // float_x4 a;
    // a[1] = 2.f;
    struct RegisterAccessWrapper
    {
        __m128& val;
        int i;

        void operator=(float x)
        {
            val[i] = x;
        }

        operator float() noexcept
        {
            return val[i];
        }
    };

public:
    __finl RegisterAccessWrapper operator[](int n)
    {
        RegisterAccessWrapper raw = { s, n };
        return raw;
    }

    __finl const float operator[](int n) const
    {
        return s[n];
    }

#elif _MSC_VER
    // on msvc returning a ref to a sub-register is possible
    __finl float& operator[](int n)
    {
        return s.m128_f32[n];
    }

    __finl const float operator[](int n) const
    {
        return s.m128_f32[n];
    }

#endif
};

__finl float_x4 __vecc float_x4_from_float(float x)
{
    return _mm_set1_ps(x);
}

__finl float_x4 __vecc float_x4_load_aligned(const float* x)
{
    return _mm_load_ps(x);
}

__finl void __vecc store_aligned(const float_x4& a, float* x)
{
    _mm_store_ps(x, a.s);
}

__finl float_x4 __vecc unzip1(const float_x4& a, const float_x4& b)
{
    return _mm_shuffle_ps(a.s, b.s, _MM_SHUFFLE(2, 0, 2, 0));
}

__finl float_x4 __vecc unzip2(const float_x4& a, const float_x4& b)
{
    return _mm_shuffle_ps(a.s, b.s, _MM_SHUFFLE(3, 1, 3, 1));
}

__finl float_x4 __vecc operator+(float_x4 a, float_x4 b)
{
    return _mm_add_ps(a.s, b.s);
}

__finl float_x4 __vecc operator-(float_x4 a, float_x4 b)
{
    return _mm_sub_ps(a.s, b.s);
}

__finl float_x4 __vecc operator*(float_x4 a, float_x4 b)
{
    return _mm_mul_ps(a.s, b.s);
}

__finl float_x4 __vecc sqrt(const float_x4& a)
{
    return _mm_sqrt_ps(a.s);
}

__finl float __vecc rint(float x)
{
    __m128i A = _mm_cvtps_epi32(_mm_set_ss(x));
    return _mm_cvtss_f32(_mm_cvtepi32_ps(A));
}

__finl float_x4 __vecc rint(const float_x4& a)
{
    __m128i A = _mm_cvtps_epi32(a.s);
    return _mm_cvtepi32_ps(A);
}
} // namespace staffpad::audio::simd
