#pragma once

#include "irmspainter.h"

namespace au::projectscene {
class MinMaxRMSPainter : public IRMSPainter
{
public:
    MinMaxRMSPainter() = default;
    void paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style, const au::au3::Au3WaveTrack& track,
               const au::au3::Au3WaveClip& clip) override;
};
}
