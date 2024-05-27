#include "waveview.h"

#include <QPainter>

#include "../timeline/timelinecontext.h"

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
    params.geometry.clipHeight = height();
    params.geometry.clipWidth = width();
    params.geometry.relClipLeft = m_clipLeft;
    params.geometry.frameLeft = m_context->frameStartTime() * m_context->zoom();
    params.geometry.frameWidth = (m_context->frameEndTime() - m_context->frameStartTime()) * m_context->zoom();

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

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, &WaveView::onFrameTimeChanged);
    }

    emit timelineContextChanged();
}

void WaveView::onFrameTimeChanged()
{
    update();
}

double WaveView::clipLeft() const
{
    return m_clipLeft;
}

void WaveView::setClipLeft(double newClipLeft)
{
    if (qFuzzyCompare(m_clipLeft, newClipLeft)) {
        return;
    }
    m_clipLeft = newClipLeft;
    emit clipLeftChanged();

    update();
}
