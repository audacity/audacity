/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramview.h"

namespace au::projectscene {
SpectrogramView::SpectrogramView(QQuickItem* parent)
    : AbstractClipView(parent)
{
}

void SpectrogramView::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }
    m_context = newContext;
    update();
}

void SpectrogramView::paint(QPainter* painter)
{
    if (!m_context) {
        return;
    }

    const auto project = globalContext()->currentProject();

    const ZoomInfo zoomInfo { m_context->zoom(), m_context->frameStartTime(), m_context->frameEndTime() };
    const SelectedRegion selectedRegion { m_clipTime.selectionStartTime, m_clipTime.selectionEndTime };

    PaintParams params;
    params.geometry.height = height();
    params.geometry.width = width();
    params.geometry.left = 0.0;
    params.zoom = m_context->zoom();
    params.fromTime = (m_clipTime.itemStartTime - m_clipTime.startTime);
    params.toTime = params.fromTime + (m_clipTime.itemEndTime - m_clipTime.startTime);
    params.selectionStartTime = m_clipTime.selectionStartTime;
    params.selectionEndTime = m_clipTime.selectionEndTime;

    const WaveMetrics metrics = wavepainterutils::getWaveMetrics(project, m_clipKey.key, params);
    spectrogramPainter()->paint(*painter, m_clipKey.key, metrics, zoomInfo, selectedRegion);
}
}
