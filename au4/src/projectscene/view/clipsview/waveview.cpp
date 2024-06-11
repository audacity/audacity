/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include <QPainter>

#include "draw/types/color.h"

#include "../timeline/timelinecontext.h"

#include "log.h"

using namespace au::projectscene;

static const QColor BACKGRAUND_COLOR = QColor(255, 255, 255);
static const QColor SAMPLES_BASE_COLOR = QColor(0, 0, 0);
static const QColor RMS_BASE_COLOR = QColor(255, 255, 255);

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

    if (m_clipSelected) {
        params.style.blankBrush = muse::draw::blendQColors(BACKGRAUND_COLOR, m_clipColor, 0.9);
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.6);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
    } else {
        params.style.blankBrush = muse::draw::blendQColors(BACKGRAUND_COLOR, m_clipColor, 0.8);
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
    }

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

QColor WaveView::clipColor() const
{
    return m_clipColor;
}

void WaveView::setClipColor(const QColor& newClipColor)
{
    if (m_clipColor == newClipColor) {
        return;
    }
    m_clipColor = newClipColor;
    emit clipColorChanged();
}

bool WaveView::clipSelected() const
{
    return m_clipSelected;
}

void WaveView::setClipSelected(bool newClipSelected)
{
    if (m_clipSelected == newClipSelected) {
        return;
    }
    m_clipSelected = newClipSelected;
    emit clipSelectedChanged();

    update();
}
