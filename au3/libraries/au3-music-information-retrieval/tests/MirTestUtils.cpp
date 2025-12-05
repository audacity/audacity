/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirTestUtils.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MirTestUtils.h"

#define USE_FILESYSTEM (__has_include(< filesystem >) && _WIN32)

#if USE_FILESYSTEM
#   include <filesystem>
#endif

#include <array>
#include <cmath>
#include <iomanip>
#include <iostream>

namespace MIR {
void ProgressBar(int width, int percent)
{
    int progress = (width * percent) / 100;
    std::cout << "[";
    for (int i = 0; i < width; ++i) {
        if (i < progress) {
            std::cout << "=";
        } else {
            std::cout << " ";
        }
    }
    std::cout << "] " << std::setw(3) << percent << "%\r";
    std::cout.flush();
}

OctaveError GetOctaveError(double expected, double actual)
{
    constexpr std::array<double, 5> factors { 1., 2., .5, 3., 1. / 3 };
    std::vector<OctaveError> octaveErrors;
    std::transform(
        factors.begin(), factors.end(), std::back_inserter(octaveErrors),
        [&](double factor) {
        const auto remainder = std::log2(factor * actual / expected);
        return OctaveError { factor, remainder };
    });
    return *std::min_element(
        octaveErrors.begin(), octaveErrors.end(),
        [](const auto& a, const auto& b) {
        return std::abs(a.remainder) < std::abs(b.remainder);
    });
}
} // namespace MIR
