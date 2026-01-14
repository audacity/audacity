#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "iconnectingdotspainter.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class ConnectingDotsPainter : public IConnectingDotsPainter, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext{ this };

public:
    ConnectingDotsPainter(const muse::modularity::ContextPtr& ctx) : muse::Injectable(ctx) {}
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) override;
};
}
