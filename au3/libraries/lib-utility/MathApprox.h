/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MathApprox.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <cstdint>

/*!
 * @brief Approximates the base-2 logarithm of a float to two decimal places,
 * adapted from https://stackoverflow.com/a/28730362
 *
 * @details Accuracy claim is demonstated in MathApproxTest.cpp, and coincides
 * with @Paul-Licameli's analysis (copied from conversation):
 * "If x == 1.0f, then log2 before the += correction actually becomes -1 --
 * because a zero exponent of two is represented by 127 in the eight exponent
 * bits, but one more was subtracted. > u.x by that point has been normalized to
 * the range [1.0f, 2.0f), by changing the exponent, multiplying it by an
 * integer power of 2. > So the correction must add 1 -- so that log2(1) comes
 * out (approximately) zero -- but then, add a quadratic polynomial in u.x,
 * a*x*x + b*x + c, where a is -0.3358287811f, b is 2, c is -1.65871759316667f
 * (the magic number as written, minus 1) > Evaluate that at x = 1, and you
 * don't get exactly 0 !  You get a little off.  It's correct to only two
 * decimal places.  Curious why they chose it that way."
 */
constexpr float FastLog2(float x)
{
    static_assert(sizeof(float) == sizeof(int32_t));
    union
    {
        float val;
        int32_t x;
    } u = { x };
    auto log_2 = (float)(((u.x >> 23) & 255) - 128);
    u.x &= ~(255 << 23);
    u.x += 127 << 23;
    log_2 += ((-0.3358287811f) * u.val + 2.0f) * u.val - 0.65871759316667f;
    return log_2;
}

static constexpr float log2ToDb = 20 / 3.321928094887362f;
