/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../spectrogramtypes.h"
#include "./WaveMetrics.h"

class SpectrogramSettings;

namespace au::projectscene {
struct SpectrogramGlobalContext {
    const WaveMetrics& metrics;
    const ZoomInfo& zoomInfo;
    const SelectedRegion& selectedRegion;
};

struct SpectrogramTrackContext {
    ::SpectrogramSettings& settings;
    const bool trackIsSelected;
    const float minFreq;
    const float maxFreq;
    const double leftRightHeightRatio;
};
}
