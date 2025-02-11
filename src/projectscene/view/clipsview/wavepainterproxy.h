#pragma once

#include "iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3/connectingdotspainter.h"
#include "au3/minmaxrmspainter.h"
#include "au3/samplespainter.h"

namespace au::projectscene {
class WavePainterProxy : public IWavePainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    WavePainterProxy() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params, std::optional<PlotType> plotType) override;

private:
    ConnectingDotsPainter m_connectingDotsPainter{};
    MinMaxRMSPainter m_minMaxRMSPainter{};
    SamplesPainter m_samplesPainter{};
};
}
