/* SPDX-License-Identifier: zlib */
/*
 * This code is cleaned up and modernized version of https://github.com/to-miz/sse_mathfun_extension
 */

#pragma once

#include <emmintrin.h>
#include <xmmintrin.h>

#include <array>
#include <complex>
#include <type_traits>
#include <memory>
#include <utility>

namespace simd_complex_conversions {
// Before C++20 there is no standard and correct way to implement bit_cast.
// It was verified that MSVC generates the correct code for the current use
// cases
namespace details {
template<class To, class From>
std::enable_if_t<
    sizeof(To) == sizeof(From) && std::is_trivially_copyable_v<From>
    && std::is_trivially_copyable_v<To>,
    To>
// constexpr support needs compiler magic
bit_cast(const From& src) noexcept
{
    static_assert(
        std::is_trivially_constructible_v<To>,
        "This implementation additionally requires "
        "destination type to be trivially constructible");

    To dst;
    std::memcpy(&dst, &src, sizeof(To));
    return dst;
}

constexpr float PIF = 3.141592653589793238f;
constexpr float PIO2F = 1.5707963267948966192f;

constexpr float cephes_PIF = 3.141592653589793238f;
constexpr float cephes_PIO2F = 1.5707963267948966192f;
constexpr float cephes_PIO4F = 0.7853981633974483096f;
constexpr float cephes_FOPI = 1.27323954473516f; // 4 / M_PI
constexpr float minus_cephes_DP1 = -0.78515625f;
constexpr float minus_cephes_DP2 = -2.4187564849853515625e-4f;
constexpr float minus_cephes_DP3 = -3.77489497744594108e-8f;
constexpr float sincof_p0 = -1.9515295891e-4f;
constexpr float sincof_p1 = 8.3321608736e-3f;
constexpr float sincof_p2 = -1.6666654611e-1f;
constexpr float coscof_p0 = 2.443315711809948e-005f;
constexpr float coscof_p1 = -1.388731625493765e-003f;
constexpr float coscof_p2 = 4.166664568298827e-002f;

constexpr float atancof_p0 = 8.05374449538e-2f;
constexpr float atancof_p1 = 1.38776856032e-1f;
constexpr float atancof_p2 = 1.99777106478e-1f;
constexpr float atancof_p3 = 3.33329491539e-1f;

static const float sign_mask = bit_cast<float>(0x80000000);
static const float inv_sign_mask = bit_cast<float>(~0x80000000);
} // namespace details

inline __m128 atan_ps(__m128 x)
{
    using namespace details;

    __m128 sign_bit, y;

    sign_bit = x;
    /* take the absolute value */
    x = _mm_and_ps(x, _mm_set1_ps(inv_sign_mask));
    /* extract the sign bit (upper one) */
    sign_bit = _mm_and_ps(sign_bit, _mm_set1_ps(sign_mask));

    /* range reduction, init x and y depending on range */
    /* x > 2.414213562373095 */
    __m128 cmp0 = _mm_cmpgt_ps(x, _mm_set1_ps(2.414213562373095f));
    /* x > 0.4142135623730950 */
    __m128 cmp1 = _mm_cmpgt_ps(x, _mm_set1_ps(0.4142135623730950f));

    /* x > 0.4142135623730950 && !( x > 2.414213562373095 ) */
    __m128 cmp2 = _mm_andnot_ps(cmp0, cmp1);

    /* -( 1.0/x ) */
    __m128 y0 = _mm_and_ps(cmp0, _mm_set1_ps(cephes_PIO2F));
    __m128 x0 = _mm_div_ps(_mm_set1_ps(1.0f), x);
    x0 = _mm_xor_ps(x0, _mm_set1_ps(sign_mask));

    __m128 y1 = _mm_and_ps(cmp2, _mm_set1_ps(cephes_PIO4F));
    /* (x-1.0)/(x+1.0) */
    __m128 x1_o = _mm_sub_ps(x, _mm_set1_ps(1.0f));
    __m128 x1_u = _mm_add_ps(x, _mm_set1_ps(1.0f));
    __m128 x1 = _mm_div_ps(x1_o, x1_u);

    __m128 x2 = _mm_and_ps(cmp2, x1);
    x0 = _mm_and_ps(cmp0, x0);
    x2 = _mm_or_ps(x2, x0);
    cmp1 = _mm_or_ps(cmp0, cmp2);
    x2 = _mm_and_ps(cmp1, x2);
    x = _mm_andnot_ps(cmp1, x);
    x = _mm_or_ps(x2, x);

    y = _mm_or_ps(y0, y1);

    __m128 zz = _mm_mul_ps(x, x);
    __m128 acc = _mm_set1_ps(atancof_p0);
    acc = _mm_mul_ps(acc, zz);
    acc = _mm_sub_ps(acc, _mm_set1_ps(atancof_p1));
    acc = _mm_mul_ps(acc, zz);
    acc = _mm_add_ps(acc, _mm_set1_ps(atancof_p2));
    acc = _mm_mul_ps(acc, zz);
    acc = _mm_sub_ps(acc, _mm_set1_ps(atancof_p3));
    acc = _mm_mul_ps(acc, zz);
    acc = _mm_mul_ps(acc, x);
    acc = _mm_add_ps(acc, x);
    y = _mm_add_ps(y, acc);

    /* update the sign */
    y = _mm_xor_ps(y, sign_bit);

    return y;
}

inline __m128 atan2_ps(__m128 y, __m128 x)
{
    using namespace details;

    __m128 zero = _mm_setzero_ps();
    __m128 x_eq_0 = _mm_cmpeq_ps(x, zero);
    __m128 x_gt_0 = _mm_cmpgt_ps(x, zero);
    __m128 x_le_0 = _mm_cmple_ps(x, zero);
    __m128 y_eq_0 = _mm_cmpeq_ps(y, zero);
    __m128 x_lt_0 = _mm_cmplt_ps(x, zero);
    __m128 y_lt_0 = _mm_cmplt_ps(y, zero);

    __m128 zero_mask = _mm_and_ps(x_eq_0, y_eq_0);
    __m128 zero_mask_other_case = _mm_and_ps(y_eq_0, x_gt_0);
    zero_mask = _mm_or_ps(zero_mask, zero_mask_other_case);

    __m128 pio2_mask = _mm_andnot_ps(y_eq_0, x_eq_0);
    __m128 pio2_mask_sign = _mm_and_ps(y_lt_0, _mm_set1_ps(sign_mask));
    __m128 pio2_result = _mm_set1_ps(cephes_PIO2F);
    pio2_result = _mm_xor_ps(pio2_result, pio2_mask_sign);
    pio2_result = _mm_and_ps(pio2_mask, pio2_result);

    __m128 pi_mask = _mm_and_ps(y_eq_0, x_lt_0);
    __m128 pi = _mm_set1_ps(cephes_PIF);
    __m128 pi_result = _mm_and_ps(pi_mask, pi);

    __m128 swap_sign_mask_offset = _mm_and_ps(x_lt_0, y_lt_0);
    swap_sign_mask_offset
        =_mm_and_ps(swap_sign_mask_offset, _mm_set1_ps(sign_mask));

    __m128 offset0 = _mm_setzero_ps();
    __m128 offset1 = _mm_set1_ps(cephes_PIF);
    offset1 = _mm_xor_ps(offset1, swap_sign_mask_offset);

    __m128 offset = _mm_andnot_ps(x_lt_0, offset0);
    offset = _mm_and_ps(x_lt_0, offset1);

    __m128 arg = _mm_div_ps(y, x);
    __m128 atan_result = atan_ps(arg);
    atan_result = _mm_add_ps(atan_result, offset);

    /* select between zero_result, pio2_result and atan_result */

    __m128 result = _mm_andnot_ps(zero_mask, pio2_result);
    atan_result = _mm_andnot_ps(zero_mask, atan_result);
    atan_result = _mm_andnot_ps(pio2_mask, atan_result);
    result = _mm_or_ps(result, atan_result);
    result = _mm_or_ps(result, pi_result);

    return result;
}

inline std::pair<__m128, __m128> sincos_ps(__m128 x)
{
    using namespace details;
    __m128 xmm1, xmm2, xmm3 = _mm_setzero_ps(), sign_bit_sin, y;
    __m128i emm0, emm2, emm4;

    sign_bit_sin = x;
    /* take the absolute value */
    x = _mm_and_ps(x, _mm_set1_ps(inv_sign_mask));
    /* extract the sign bit (upper one) */
    sign_bit_sin = _mm_and_ps(sign_bit_sin, _mm_set1_ps(sign_mask));

    /* scale by 4/Pi */
    y = _mm_mul_ps(x, _mm_set1_ps(cephes_FOPI));

    /* store the integer part of y in emm2 */
    emm2 = _mm_cvttps_epi32(y);

    /* j=(j+1) & (~1) (see the cephes sources) */
    emm2 = _mm_add_epi32(emm2, _mm_set1_epi32(1));
    emm2 = _mm_and_si128(emm2, _mm_set1_epi32(~1));
    y = _mm_cvtepi32_ps(emm2);

    emm4 = emm2;

    /* get the swap sign flag for the sine */
    emm0 = _mm_and_si128(emm2, _mm_set1_epi32(4));
    emm0 = _mm_slli_epi32(emm0, 29);
    __m128 swap_sign_bit_sin = _mm_castsi128_ps(emm0);

    /* get the polynom selection mask for the sine*/
    emm2 = _mm_and_si128(emm2, _mm_set1_epi32(2));
    emm2 = _mm_cmpeq_epi32(emm2, _mm_setzero_si128());
    __m128 poly_mask = _mm_castsi128_ps(emm2);

    /* The magic pass: "Extended precision modular arithmetic"
       x = ((x - y * DP1) - y * DP2) - y * DP3; */
    xmm1 = _mm_set1_ps(minus_cephes_DP1);
    xmm2 = _mm_set1_ps(minus_cephes_DP2);
    xmm3 = _mm_set1_ps(minus_cephes_DP3);
    xmm1 = _mm_mul_ps(y, xmm1);
    xmm2 = _mm_mul_ps(y, xmm2);
    xmm3 = _mm_mul_ps(y, xmm3);
    x = _mm_add_ps(x, xmm1);
    x = _mm_add_ps(x, xmm2);
    x = _mm_add_ps(x, xmm3);

    emm4 = _mm_sub_epi32(emm4, _mm_set1_epi32(2));
    emm4 = _mm_andnot_si128(emm4, _mm_set1_epi32(4));
    emm4 = _mm_slli_epi32(emm4, 29);
    __m128 sign_bit_cos = _mm_castsi128_ps(emm4);

    sign_bit_sin = _mm_xor_ps(sign_bit_sin, swap_sign_bit_sin);

    /* Evaluate the first polynom  (0 <= x <= Pi/4) */
    __m128 z = _mm_mul_ps(x, x);
    y = _mm_set1_ps(coscof_p0);

    y = _mm_mul_ps(y, z);
    y = _mm_add_ps(y, _mm_set1_ps(coscof_p1));
    y = _mm_mul_ps(y, z);
    y = _mm_add_ps(y, _mm_set1_ps(coscof_p2));
    y = _mm_mul_ps(y, z);
    y = _mm_mul_ps(y, z);
    __m128 tmp = _mm_mul_ps(z, _mm_set1_ps(0.5f));
    y = _mm_sub_ps(y, tmp);
    y = _mm_add_ps(y, _mm_set1_ps(1));

    /* Evaluate the second polynom  (Pi/4 <= x <= 0) */

    __m128 y2 = _mm_set1_ps(sincof_p0);
    y2 = _mm_mul_ps(y2, z);
    y2 = _mm_add_ps(y2, _mm_set1_ps(sincof_p1));
    y2 = _mm_mul_ps(y2, z);
    y2 = _mm_add_ps(y2, _mm_set1_ps(sincof_p2));
    y2 = _mm_mul_ps(y2, z);
    y2 = _mm_mul_ps(y2, x);
    y2 = _mm_add_ps(y2, x);

    /* select the correct result from the two polynoms */
    xmm3 = poly_mask;
    __m128 ysin2 = _mm_and_ps(xmm3, y2);
    __m128 ysin1 = _mm_andnot_ps(xmm3, y);
    y2 = _mm_sub_ps(y2, ysin2);
    y = _mm_sub_ps(y, ysin1);

    xmm1 = _mm_add_ps(ysin1, ysin2);
    xmm2 = _mm_add_ps(y, y2);

    /* update the sign */
    return std::make_pair(
        _mm_xor_ps(xmm1, sign_bit_sin), _mm_xor_ps(xmm2, sign_bit_cos));
}

inline float atan2_ss(float y, float x)
{
    return _mm_cvtss_f32(atan2_ps(_mm_set_ss(y), _mm_set_ss(x)));
}

inline std::pair<float, float> sincos_ss(float angle)
{
    auto res = sincos_ps(_mm_set_ss(angle));
    return std::make_pair(_mm_cvtss_f32(res.first), _mm_cvtss_f32(res.second));
}

inline __m128 norm(__m128 x, __m128 y)
{
    return _mm_add_ps(_mm_mul_ps(x, x), _mm_mul_ps(y, y));
}

inline float sqrt_ss(float x)
{
    __m128 sse_value = _mm_set_ss(x);
    sse_value = _mm_sqrt_ss(sse_value);
    return _mm_cvtss_f32(sse_value);
}

template<typename fnc>
void perform_parallel_simd_aligned(
    const std::complex<float>* input, float* output, int n, const fnc& f)
{
    for (int i = 0; i <= n - 4; i += 4) {
        // Safe according to C++ standard
        auto p1 = _mm_load_ps(reinterpret_cast<const float*>(input + i));
        auto p2 = _mm_load_ps(reinterpret_cast<const float*>(input + i + 2));

        // p1 = {real(c1), imag(c1), real(c2), imag(c2)}
        // p2 = {real(c3), imag(c3), real(c4), imag(c4)}

        auto rp = _mm_shuffle_ps(p1, p2, _MM_SHUFFLE(2, 0, 2, 0));
        auto ip = _mm_shuffle_ps(p1, p2, _MM_SHUFFLE(3, 1, 3, 1));

        __m128 out;
        f(rp, ip, out);

        _mm_store_ps(output + i, out);
    }
    // deal with last partial packet
    for (int i = n & (~3); i < n; ++i) {
        __m128 out;
        f(_mm_set_ss(real(input[i])), _mm_set_ss(imag(input[i])), out);
        output[i] = _mm_cvtss_f32(out);
    }
}

inline void rotate_parallel_simd_aligned(
    const float* oldPhase, const float* newPhase, std::complex<float>* output,
    int n)
{
    for (int i = 0; i <= n - 4; i += 4) {
        auto [sin, cos] = sincos_ps(
            oldPhase
            ? _mm_sub_ps(_mm_load_ps(newPhase + i), _mm_load_ps(oldPhase + i))
            : _mm_load_ps(newPhase + i));

        // Safe according to C++ standard
        auto p1 = _mm_load_ps(reinterpret_cast<float*>(output + i));
        auto p2 = _mm_load_ps(reinterpret_cast<float*>(output + i + 2));

        // p1 = {real(c1), imag(c1), real(c2), imag(c2)}
        // p2 = {real(c3), imag(c3), real(c4), imag(c4)}

        auto rp = _mm_shuffle_ps(p1, p2, _MM_SHUFFLE(2, 0, 2, 0));
        auto ip = _mm_shuffle_ps(p1, p2, _MM_SHUFFLE(3, 1, 3, 1));

        // We need to calculate (rp, ip) * (cos, sin) -> (rp*cos - ip*sin, rp*sin + ip*cos)

        auto out_rp = _mm_sub_ps(_mm_mul_ps(rp, cos), _mm_mul_ps(ip, sin));
        auto out_ip = _mm_add_ps(_mm_mul_ps(rp, sin), _mm_mul_ps(ip, cos));

        p1 = _mm_unpacklo_ps(out_rp, out_ip);
        p2 = _mm_unpackhi_ps(out_rp, out_ip);

        _mm_store_ps(reinterpret_cast<float*>(output + i), p1);
        _mm_store_ps(reinterpret_cast<float*>(output + i + 2), p2);
    }
    // deal with last partial packet
    for (int i = n & (~3); i < n; ++i) {
        const auto theta = oldPhase ? newPhase[i] - oldPhase[i] : newPhase[i];
        output[i] *= std::complex<float>(cosf(theta), sinf(theta));
    }
}
} // namespace simd_complex_conversions
