#include "waveview.h"

#include <QPainter>

#include "timelinecontext.h"

#include "log.h"

using namespace au::projectscene;

WaveView::WaveView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAcceptHoverEvents(true);
}

WaveView::~WaveView()
{
}

void WaveView::setClipKey(const ClipKey& newClipKey)
{
    m_clipKey = newClipKey;
    emit clipKeyChanged();

    update();
}

void WaveView::paint(QPainter* painter)
{
    au3::IAu3WavePainter::Params params;
    params.viewRect = QRect(0, 0, width(), height());
    params.zoom = m_context->zoom();

    const WaveStyle& style = configuration()->waveStyle();

    params.style.blankBrush = style.blankBrush;
    params.style.samplePen = style.samplePen;
    params.style.sampleBrush = style.sampleBrush;
    params.style.rmsPen = style.rmsPen;
    params.style.clippedPen = style.clippedPen;
    params.style.highlight = style.highlight;

    wavePainter()->paint(*painter, m_clipKey.key, params);
}

ClipKey WaveView::clipKey() const
{
    return m_clipKey;
}

TimelineContext* WaveView::timelineContext() const
{
    return m_context;
}

void WaveView::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    //! TODO Subscribe on context props changes
    m_context = newContext;
    emit timelineContextChanged();
}
