/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include <QPainter>
#include <QElapsedTimer>

#include "draw/types/color.h"

#include "../timeline/timelinecontext.h"

#include "log.h"

using namespace au::projectscene;

static const QColor BACKGROUND_COLOR = QColor(255, 255, 255);
static const QColor SAMPLES_BASE_COLOR = QColor(0, 0, 0);
static const QColor RMS_BASE_COLOR = QColor(255, 255, 255);

WaveView::WaveView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
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
    // QElapsedTimer timer;
    // timer.start();

    IWavePainter::Params params;
    params.geometry.height = height();
    params.geometry.width = width();
    params.geometry.left = 0.0;

    params.zoom = m_context->zoom();
    params.fromTime = (m_clipTime.itemStartTime - m_clipTime.clipStartTime);
    params.toTime = params.fromTime + (m_clipTime.itemEndTime - m_clipTime.itemStartTime);

    // LOGDA() << " geometry.height: " << params.geometry.height
    //         << " geometry.width: " << params.geometry.width
    //         << " geometry.left: " << params.geometry.left
    //         << " zoom: " << params.zoom
    //         << " fromTime: " << params.fromTime
    //         << " toTime: " << params.toTime;

    if (m_clipSelected) {
        params.style.blankBrush = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8);
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
    } else {
        params.style.blankBrush = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.9);
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.6);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
    }

    painter->fillRect(0, 0, width(), height(), params.style.blankBrush);

    wavePainter()->paint(*painter, m_clipKey.key, params);

    //LOGDA() << timer.elapsed() << " ms";
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

ClipTime WaveView::clipTime() const
{
    return m_clipTime;
}

void WaveView::setClipTime(const ClipTime& newClipTime)
{
    if (m_clipTime == newClipTime) {
        return;
    }
    m_clipTime = newClipTime;
    emit clipTimeChanged();

    update();
}
