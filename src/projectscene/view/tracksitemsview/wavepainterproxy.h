#pragma once

#include "iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3/iconnectingdotspainter.h"
#include "au3/iminmaxrmspainter.h"
#include "au3/isamplespainter.h"

namespace au::projectscene {
class WavePainterProxy : public IWavePainter, public muse::Contextable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IConnectingDotsPainter> connectingDotsPainter{ this };
    muse::ContextInject<IMinMaxRMSPainter> minMaxRMSPainter{ this };
    muse::ContextInject<ISamplesPainter> samplesPainter{ this };

public:
    WavePainterProxy(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params, std::optional<PlotType> plotType) override;
};
}
