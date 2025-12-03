/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"
#include "internal/spectrogramutils.h"

class SpectrogramSettings;

namespace au::spectrogram {
struct SpectrogramGlobalContext {
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
