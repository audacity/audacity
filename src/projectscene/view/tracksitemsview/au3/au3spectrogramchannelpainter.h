/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ispectrogrampainter.h"
#include "../tracksitemsviewtypes.h"

#include "au3wrap/au3types.h"
#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

#include <QPainter>

namespace au::projectscene {
struct WaveMetrics;

class Au3SpectrogramChannelPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SpectrogramChannelPainter(std::weak_ptr<au3::Au3Project> au3Project);

    struct Params : PaintParams {
        Params(SpectrogramSettings& settings, const SelectedRegion& selectedRegion, const ZoomInfo& zoomInfo, bool trackIsSelected);

        SpectrogramSettings& settings;
        const ZoomInfo& zoomInfo;
        const SelectedRegion& selectedRegion;
        const bool trackIsSelected;
    };

    void paint(QPainter&, WaveClipChannel&, const WaveChannel&, const WaveMetrics&, const Params&);

private:
    std::weak_ptr<au3::Au3Project> m_au3Project; // TODO check if still needed when done
};
}
