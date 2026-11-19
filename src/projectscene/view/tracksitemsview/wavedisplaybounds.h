/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <algorithm>
#include <cmath>
#include <utility>

#include "framework/global/types/ratio.h"

#include "iwavepainter.h"

namespace au::projectscene {
//! A sample value on the dB display scale: 0 at -dbRange dB and below, 1 at 0 dB, keeping the sign
inline float toDbDisplayValue(float value, float dbRange)
{
    if (value == 0.0f) {
        return value;
    }
    const float sign = value >= 0.0f ? 1.0f : -1.0f;
    return std::max((muse::linear_to_db(value) + dbRange) / dbRange, 0.0f) * sign;
}

//! The y of a display value in a channel of the given height, from its top
inline float displayValueToY(float value, float displayMin, float displayMax, float height)
{
    if (displayMax == displayMin) {
        return height * 0.5f;
    }
    return (displayMax - value) / (displayMax - displayMin) * (height - 1.0f);
}

//! The display bounds in the coordinates samples are mapped to, which are dB-scaled in a dB view
inline std::pair<float, float> waveDisplayBounds(const IWavePainter::Params& params)
{
    if (params.isLinear) {
        return params.displayBounds;
    }

    const float dbRange = std::abs(static_cast<float>(params.dbRange));
    return { toDbDisplayValue(params.displayBounds.first, dbRange), toDbDisplayValue(params.displayBounds.second, dbRange) };
}
}
