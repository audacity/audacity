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

    const QRect visibleSubrect = clipRect().toRect();
    const int xBegin = std::max(visibleSubrect.left() - m_timelineIndentWidth, 0);
    const int xEnd = visibleSubrect.right() + 1;
    const spectrogram::ClipInfo clipInfo {
        static_cast<int>(m_clipKey.key.itemId),
        static_cast<int>(m_clipKey.key.trackId),
        xBegin,
        xEnd,
    };

    spectrogramPainter()->paintClip(*painter, clipInfo, height(), viewportT0, viewportT1, m_context->zoom(), selectedRegion);
}
}
