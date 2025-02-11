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
        m_connectingDotsPainter.paint(painter, clipKey, params);
        break;
    case PlotType::MinMaxRMS:
        m_minMaxRMSPainter.paint(painter, clipKey, params);
        break;
    case PlotType::Stem:
        m_samplesPainter.paint(painter, clipKey, params);
        break;
    default:
        break;
    }
}
}

/*void WaveView::handleSnapChange(PlotType plotType)
{
    if (plotType == PlotType::Stem) {
        if (!m_snap) {
            m_snap = globalContext()->currentProject()->viewState()->getSnap();
        }
        globalContext()->currentProject()->viewState()->setSnap(Snap { SnapType::Samples, true, false });
    } else {
        globalContext()->currentProject()->viewState()->setSnap(m_snap.value_or(Snap { SnapType::Bar, false, false }));
        m_snap.reset();
    }
}*/
