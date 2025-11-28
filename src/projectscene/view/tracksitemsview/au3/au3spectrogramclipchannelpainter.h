/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ispectrogrampainter.h"
#include "../tracksitemsviewtypes.h"
#include "./au3spectrogramtypes.h"

#include "au3wrap/au3types.h"
#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

#include <QPainter>

namespace au::projectscene {
struct WaveMetrics;

class Au3SpectrogramClipChannelPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SpectrogramClipChannelPainter(std::shared_ptr<WaveClipChannel>);

    void paint(QPainter&, const SpectrogramGlobalContext&, const SpectrogramTrackContext&);

private:
    const std::shared_ptr<WaveClipChannel> m_waveClipChannel;
};
}
