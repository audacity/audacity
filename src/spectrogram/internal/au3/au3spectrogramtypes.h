/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"
#include "projectscene/view/tracksitemsview/au3/WaveMetrics.h"

class SpectrogramSettings;

namespace au::spectrogram {
struct SpectrogramGlobalContext {
    const projectscene::WaveMetrics& metrics;
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
