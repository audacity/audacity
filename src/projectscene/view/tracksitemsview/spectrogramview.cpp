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
    const ZoomInfo zoomInfo { m_context->zoom(), m_context->frameStartTime() };
    const auto& selectionCtrl = *selectionController();
    const SelectedRegion selectedRegion { selectionCtrl.selectionStartTime(), selectionCtrl.selectionEndTime(),
                                          0.0, 0.0 /* freq selection not supported yet */ };
    spectrogramPainter()->paint(*painter, m_clipKey.key, zoomInfo, selectedRegion);
}
}
