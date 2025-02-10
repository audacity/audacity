#pragma once

#include "iwavepainter.h"

namespace au::projectscene {
class ConnectingDotsPainter : public IWavePainter
{
public:
    ConnectingDotsPainter() = default;
    void paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style, const au::au3::Au3WaveTrack& track,
               const au::au3::Au3WaveClip& clip) override;
};
}
