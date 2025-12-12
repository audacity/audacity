/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

namespace au::spectrogram {
class SpectrogramColors
{
public:

    enum ColorGradientChoice {
        ColorGradientUnselected = 0,
        ColorGradientTimeSelected,
        ColorGradientTimeAndFrequencySelected,
        ColorGradientEdge,

        ColorGradientTotal // keep me last
    };

    static void PreComputeGradient();

    static bool gradient_inited;
    static const int colorSchemes = static_cast<int>(SpectrogramColorScheme::_count);
    static const int gradientSteps = 256;
    static unsigned char gradient_pre[ColorGradientTotal][colorSchemes][gradientSteps][3];
};

inline void GetColorGradient(float value,
                             SpectrogramColors::ColorGradientChoice selected,
                             SpectrogramColorScheme colorScheme,
                             unsigned char* __restrict red,
                             unsigned char* __restrict green,
                             unsigned char* __restrict blue)
{
    int idx = value * (SpectrogramColors::gradientSteps - 1);

    const auto c = static_cast<int>(colorScheme);
    *red = SpectrogramColors::gradient_pre[selected][c][idx][0];
    *green = SpectrogramColors::gradient_pre[selected][c][idx][1];
    *blue = SpectrogramColors::gradient_pre[selected][c][idx][2];
}
}
