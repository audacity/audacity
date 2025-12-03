/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramview.h"

namespace au::projectscene {
SpectrogramView::SpectrogramView(QQuickItem* parent)
    : AbstractClipView(parent)
{
    setFlag(QQuickItem::ItemObservesViewport, true);
}

void SpectrogramView::setTimelineIndentWidth(int width)
{
    if (m_timelineIndentWidth == width) {
        return;
    }
    m_timelineIndentWidth = width;
    emit timelineIndentWidthChanged();
    update();
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

    const auto indentTime = m_timelineIndentWidth / m_context->zoom();
    const auto viewportT0 = m_context->frameStartTime() - indentTime;
    const auto viewportT1 = m_context->frameEndTime();
    const spectrogram::SelectedRegion selectedRegion { m_clipTime.selectionStartTime, m_clipTime.selectionEndTime };

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

    const QRect visibleSubrect = clipRect().toRect();
    const int xBegin = std::max(visibleSubrect.left() - m_timelineIndentWidth, 0);
    const int xEnd = visibleSubrect.right() + 1;
    spectrogramPainter()->paintClip(*painter, xBegin, xEnd, height(), viewportT0, viewportT1,
                                    m_context->zoom(), m_clipKey.key, metrics, selectedRegion);
}
}
