/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <algorithm>
#include <cmath>
#include <utility>

#include "iwavepainter.h"

namespace au::projectscene {
//! The display bounds in the coordinates samples are mapped to, which are dB-scaled in a dB view
inline std::pair<float, float> waveDisplayBounds(const IWavePainter::Params& params)
{
    if (params.isLinear) {
        return params.displayBounds;
    }

    const float dbRange = std::abs(static_cast<float>(params.dbRange));
    auto toDB = [dbRange](float value) {
        if (value == 0.0f) {
            return value;
        }
        const float sign = value >= 0.0f ? 1.0f : -1.0f;
        const float db = 20.0f * std::log10(std::fabs(value));
        return std::max((db + dbRange) / dbRange, 0.0f) * sign;
    };
    return { toDB(params.displayBounds.first), toDB(params.displayBounds.second) };
}
}
