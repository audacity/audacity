/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"
#include "./au3spectrogramtypes.h"

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

#include "libraries/lib-wave-track/WaveClip.h"

#include <QImage>

namespace au::spectrogram {
struct WaveMetrics;

class Au3SpectrogramClipChannelPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SpectrogramClipChannelPainter(std::shared_ptr<WaveClipChannel>);

    void paint(QImage&, const ViewInfo&, const SelectedRegion&, const SpectrogramTrackContext&);

private:
    const std::shared_ptr<WaveClipChannel> m_waveClipChannel;
};
}
