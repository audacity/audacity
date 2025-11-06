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
    painter->fillRect(boundingRect(), Qt::gray);
}
}
