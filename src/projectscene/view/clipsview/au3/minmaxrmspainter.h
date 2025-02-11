#pragma once

#include "../iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class MinMaxRMSPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    MinMaxRMSPainter() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params);
};
}
