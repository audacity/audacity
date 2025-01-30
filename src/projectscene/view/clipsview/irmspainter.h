#pragma once

#include "iwavepainter.h"

#include "modularity/imoduleinterface.h"

#include "au3/WaveMetrics.h"
#include "au3wrap/au3types.h"

#include "WaveClip.h"

using Style = au::projectscene::IWavePainter::Style;

namespace au::projectscene {
class IRMSPainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRMSPainter)

public:
    virtual ~IRMSPainter() = default;
    virtual void paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style,
                       const au::au3::Au3WaveTrack& track, const au::au3::Au3WaveClip& clip) = 0;
};
}
