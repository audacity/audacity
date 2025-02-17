/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirDsp.h

  Matthieu Hodgkinson

  DSP utilities used by the Music Information Retrieval code.
  These may migrate to lib-math if needed elsewhere.

**********************************************************************/

#pragma once

#include <algorithm>
#include <functional>
#include <vector>

namespace MIR {
class MirAudioReader;
struct QuantizationFitDebugOutput;

/*!
 * @brief Get the normalized, circular auto-correlation for a signal `x` whose
 * length already is a power of two. Since the output is symmetric, only the
 * left-hand side is returned, i.e., of size `N/2 + 1`, where `N` is the power
 * of two the input was upsampled to.
 *
 * @pre `x.size()` is a power of two.
 * @post returned vector has size `x.size() / 2 + 1`.
 */
std::vector<float> GetNormalizedCircularAutocorr(const std::vector<float>& x);

std::vector<float> GetOnsetDetectionFunction(
    const MirAudioReader& audio, const std::function<void(double)>& progressCallback, QuantizationFitDebugOutput* debugInfo);
} // namespace MIR
