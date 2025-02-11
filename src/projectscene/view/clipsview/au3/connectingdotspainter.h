#pragma once

#include "../iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class ConnectingDotsPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ConnectingDotsPainter() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params);
};
}
