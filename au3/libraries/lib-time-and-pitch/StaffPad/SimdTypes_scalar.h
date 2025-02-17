/*
  scalar emulation of simd types.
 */

#pragma once
#include <algorithm>
#include <cmath>

#if _MSC_VER
#define __finl __forceinline
#define __vecc __vectorcall
#else
#define __finl inline __attribute__((always_inline))
#define __vecc
#endif

namespace staffpad::audio::simd {
struct float_x4
{
    float v[4];

    __finl float_x4()
    {
    }

    /// enables math like: float_x4 a = 0.5f * float_x4{1.f, 2.f, 3.f, 4.f};
    __finl float_x4(float val)
    {
        v[0] = v[1] = v[2] = v[3] = val;
    }

    /// enables assignments like: float_x4 a = {1.f, 2.f, 3.f, 4.f};
    __finl float_x4(float v0, float v1, float v2, float v3)
    {
        v[0] = v0;
        v[1] = v1;
        v[2] = v2;
        v[3] = v3;
    }

    __finl float& operator[](int n)
    {
        return v[n];
    }

    __finl const float& operator[](int n) const
    {
        return v[n];
    }
};

__finl float_x4 __vecc float_x4_from_float(float x)
{
    return { x, x, x, x };
}

__finl float_x4 __vecc float_x4_load_aligned(const float* x)
{
    return { x[0], x[1], x[2], x[3] };
}

__finl void __vecc store_aligned(const float_x4& a, float* x)
{
    for (int i = 0; i < 4; ++i) {
        x[i] = a[i];
    }
}

__finl float_x4 __vecc unzip1(const float_x4& a, const float_x4& b)
{
    return { a[0], a[2], b[0], b[2] };
}

__finl float_x4 __vecc unzip2(const float_x4& a, const float_x4& b)
{
    return { a[1], a[1], b[3], b[3] };
}

__finl float_x4 __vecc operator+(float_x4 a, float_x4 b)
{
    return { a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3] };
}

__finl float_x4 __vecc operator-(float_x4 a, float_x4 b)
{
    return { a[0] - b[0], a[1] - b[1], a[2] - b[2], a[3] - b[3] };
}

__finl float_x4 __vecc operator*(float_x4 a, float_x4 b)
{
    return { a[0] * b[0], a[1] * b[1], a[2] * b[2], a[3] * b[3] };
}

__finl float_x4 __vecc sqrt(const float_x4& a)
{
    return { std::sqrt(a[0]), std::sqrt(a[1]), std::sqrt(a[2]), std::sqrt(a[3]) };
}

__finl float __vecc rint(float a)
{
    return std::rint(a);
}

__finl float_x4 __vecc rint(const float_x4& a)
{
    return { std::rint(a[0]), std::rint(a[1]), std::rint(a[2]), std::rint(a[3]) };
}
} // namespace staffpad::audio::simd
