#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "iconnectingdotspainter.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class ConnectingDotsPainter : public IConnectingDotsPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ConnectingDotsPainter() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) override;
};
}
