#include "wavepainterproxy.h"

#include "au3/WaveMetrics.h"
#include "au3/wavepainterutils.h"

namespace au::projectscene {
void WavePainterProxy::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params, std::optional<PlotType> plotType)
{
    painter.setPen(Qt::NoPen);

    PlotType pType = plotType.value_or(wavepainterutils::getPlotType(globalContext()->currentProject(), clipKey, params.zoom));

    switch (pType) {
    case PlotType::ConnectingDots:
        connectingDotsPainter()->paint(painter, clipKey, params);
        break;
    case PlotType::MinMaxRMS:
        minMaxRMSPainter()->paint(painter, clipKey, params);
        break;
    case PlotType::Stem:
        samplesPainter()->paint(painter, clipKey, params);
        break;
    default:
        break;
    }
}
}
