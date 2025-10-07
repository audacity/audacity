#pragma once

#include "iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3/iconnectingdotspainter.h"
#include "au3/iminmaxrmspainter.h"
#include "au3/isamplespainter.h"

namespace au::projectscene {
class WavePainterProxy : public IWavePainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IConnectingDotsPainter> connectingDotsPainter;
    muse::Inject<IMinMaxRMSPainter> minMaxRMSPainter;
    muse::Inject<ISamplesPainter> samplesPainter;

public:
    WavePainterProxy() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params, std::optional<PlotType> plotType) override;
};
}
