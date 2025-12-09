/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirUtils.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MirUtils.h"

#include <cmath>
#include <numeric>

namespace MIR {
namespace {
constexpr auto bpmStdDev = 29.7953;
constexpr auto pi = 3.141592653589793;

constexpr bool IsPrimeDecompositionTwoThreeOnly(int n)
{
    while (n % 2 == 0) {
        n /= 2;
    }
    while (n % 3 == 0) {
        n /= 3;
    }
    return n == 1;
}

static_assert(IsPrimeDecompositionTwoThreeOnly(1));
static_assert(IsPrimeDecompositionTwoThreeOnly(2));
static_assert(IsPrimeDecompositionTwoThreeOnly(3));
static_assert(IsPrimeDecompositionTwoThreeOnly(4));
static_assert(!IsPrimeDecompositionTwoThreeOnly(5));
static_assert(IsPrimeDecompositionTwoThreeOnly(6));
static_assert(!IsPrimeDecompositionTwoThreeOnly(7));
static_assert(IsPrimeDecompositionTwoThreeOnly(8));
static_assert(IsPrimeDecompositionTwoThreeOnly(9));

// Function to generate numbers whose prime factorization contains only twos or
// threes
std::vector<int> GetPowersOf2And3(int lower, int upper)
{
    std::vector<int> result;
    for (int i = lower; i <= upper; ++i) {
        if (IsPrimeDecompositionTwoThreeOnly(i)) {
            result.push_back(i);
        }
    }
    return result;
}
} // namespace

std::vector<int> GetPossibleBarDivisors(int lower, int upper)
{
    auto result = GetPowersOf2And3(lower, upper);
    // Remove divisors that have more than two triplet levels. E.g. 3/4s are
    // okay, 3/4s with swung ryhthms too, but beyond that it's probably very rare
    // (e.g. swung 9/8 ??...)
    result.erase(
        std::remove_if(
            result.begin(), result.end(), [](int n) { return n % 27 == 0; }),
        result.end());
    return result;
}

std::vector<int> GetPeakIndices(const std::vector<float>& x)
{
    std::vector<int> peakIndices;
    for (auto j = 0; j < x.size(); ++j) {
        const auto i = j == 0 ? x.size() - 1 : j - 1;
        const auto k = j == x.size() - 1 ? 0 : j + 1;
        if (x[i] < x[j] && x[j] > x[k]) {
            peakIndices.push_back(j);
        }
    }
    return peakIndices;
}

std::vector<float> GetNormalizedHann(int size)
{
    std::vector<float> window(size);
    for (auto n = 0; n < size; ++n) {
        window[n] = .5 * (1 - std::cos(2 * pi * n / size));
    }
    const auto windowSum = std::accumulate(window.begin(), window.end(), 0.f);
    std::transform(
        window.begin(), window.end(), window.begin(),
        [windowSum](float w) { return w / windowSum; });
    return window;
}
} // namespace MIR
