/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ispectrogrampainter.h"

#include "au3wrap/au3types.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

#include <QPainter>

namespace au::projectscene {
class Au3SpectrogramChannelPainter
{
public:
    Au3SpectrogramChannelPainter(std::weak_ptr<au3::Au3Project> au3Project);

    struct Params {
        Params(SpectrogramSettings& settings,
               const SelectedRegion& selectedRegion,
               const ZoomInfo& zoomInfo,
               bool trackIsSelected)
            : settings{settings},
            selectedRegion{selectedRegion},
            zoomInfo{zoomInfo},
            trackIsSelected{trackIsSelected}
        {
        }

        SpectrogramSettings& settings;
        const ZoomInfo& zoomInfo;
        const SelectedRegion& selectedRegion;
        const bool trackIsSelected;
    };

    void paint(QPainter&, WaveClipChannel& clipChannel, const Params& params);

private:
    std::weak_ptr<au3::Au3Project> m_au3Project; // TODO check if still needed when done
};
}
