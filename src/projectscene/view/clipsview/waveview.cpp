/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include <QPainter>
#include <QElapsedTimer>

#include "draw/types/color.h"
#include "log.h"

#include "au3/wavepainterutils.h"
#include "au3/samplespainterutils.h"
#include "../timeline/timelinecontext.h"

using namespace au::projectscene;

static const QColor BACKGROUND_COLOR = QColor(255, 255, 255);
static const QColor SAMPLES_BASE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLES_HIGHLIGHT_COLOR = QColor(255, 255, 255);
static const QColor RMS_BASE_COLOR = QColor(255, 255, 255);
static const QColor CENTER_LINE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_HEAD_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_STALK_COLOR = QColor(0, 0, 0);

static const float SAMPLE_HEAD_DEFAULT_ALPHA= 0.6;
static const float SAMPLE_HEAD_CLIP_SELECTED_ALPHA = 0.8;
static const float SAMPLE_HEAD_DATA_SELECTED_ALPHA = 0.9;
static const float SAMPLE_STALK_DEFAULT_ALPHA = 0.4;
static const float SAMPLE_STALK_CLIP_SELECTED_ALPHA = 0.6;
static const float SAMPLE_STALK_DATA_SELECTED_ALPHA = 0.7;

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

IWavePainter::Params WaveView::getWavePainterParams() const
{
    IWavePainter::Params params;
    params.geometry.height = height();
    params.geometry.width = width();
    params.geometry.left = 0.0;

    params.zoom = m_context->zoom();
    params.fromTime = (m_clipTime.itemStartTime - m_clipTime.clipStartTime);
    params.toTime = params.fromTime + (m_clipTime.itemEndTime - m_clipTime.itemStartTime);
    params.selectionStartTime = m_clipTime.selectionStartTime;
    params.selectionEndTime = m_clipTime.selectionEndTime;
    params.channelHeightRatio = m_channelHeightRatio;

    if (m_clipSelected) {
        params.style.blankBrush = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8);
        params.style.normalBackground = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8);
        params.style.selectedBackground = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8);
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
        params.style.selectedSamplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_HIGHLIGHT_COLOR, 0.8);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
        params.style.centerLine = muse::draw::blendQColors(params.style.samplePen, CENTER_LINE_COLOR, 0.2);
        params.style.sampleHead = muse::draw::blendQColors(params.style.blankBrush, SAMPLE_HEAD_COLOR, SAMPLE_HEAD_CLIP_SELECTED_ALPHA);
        params.style.sampleStalk = muse::draw::blendQColors(params.style.blankBrush, SAMPLE_STALK_COLOR, SAMPLE_STALK_CLIP_SELECTED_ALPHA);
    } else {
        params.style.blankBrush = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.9);
        params.style.normalBackground = muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8);
        params.style.selectedBackground = transformColor(muse::draw::blendQColors(BACKGROUND_COLOR, m_clipColor, 0.8));
        params.style.samplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
        params.style.selectedSamplePen = muse::draw::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.75);
        params.style.rmsPen = muse::draw::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.1);
        params.style.centerLine = muse::draw::blendQColors(params.style.samplePen, CENTER_LINE_COLOR, 0.2);
        params.style.sampleHead = muse::draw::blendQColors(params.style.samplePen, SAMPLE_HEAD_COLOR, SAMPLE_HEAD_DEFAULT_ALPHA);
        params.style.sampleStalk = muse::draw::blendQColors(params.style.samplePen, SAMPLE_STALK_COLOR, SAMPLE_STALK_DEFAULT_ALPHA);
        params.style.sampleHeadSelection = muse::draw::blendQColors(params.style.samplePen, SAMPLE_HEAD_COLOR,
                                                                    SAMPLE_HEAD_DATA_SELECTED_ALPHA);
        params.style.sampleStalkSelection = muse::draw::blendQColors(params.style.samplePen, SAMPLE_STALK_COLOR,
                                                                     SAMPLE_STALK_DATA_SELECTED_ALPHA);
    }

    return params;
}

void WaveView::paint(QPainter* painter)
{
    wavePainter()->paint(*painter, m_clipKey.key, getWavePainterParams());
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
        connect(m_context, &TimelineContext::frameTimeChanged, this, &WaveView::updateView);
        connect(m_context, &TimelineContext::selectionStartTimeChanged, this, &WaveView::updateView);
        connect(m_context, &TimelineContext::selectionEndTimeChanged, this, &WaveView::updateView);
    }

    emit timelineContextChanged();
}

void WaveView::updateView()
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

    update();
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

double WaveView::channelHeightRatio() const
{
    return m_channelHeightRatio;
}

void WaveView::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
    emit channelHeightRatioChanged();
    update();
}

bool WaveView::isNearSample() const
{
    return m_isNearSample;
}

void WaveView::setIsNearSample(bool isNearSample)
{
    if (m_isNearSample == isNearSample) {
        return;
    }

    m_isNearSample = isNearSample;
    emit isNearSampleChanged();
}

QColor WaveView::transformColor(const QColor& originalColor) const
{
    int r = originalColor.red();
    int g = originalColor.green();
    int b = originalColor.blue();

    int deltaRed = (r < 240) ? 51 : (255 - r);
    int deltaGreen = (g < 240) ? 69 : (255 - g);
    int deltaBlue = 77;

    int newRed = qBound(0, r + deltaRed, 255);
    int newGreen = qBound(0, g + deltaGreen, 255);
    int newBlue = qBound(0, b + deltaBlue, 255);

    return QColor(newRed, newGreen, newBlue);
}

void WaveView::setLastMousePos(const unsigned int x, const unsigned int y)
{
    if (wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key,
                                      m_context->zoom()) != IWavePainter::PlotType::Stem) {
        return;
    }

    const auto params = getWavePainterParams();
    m_currentChannel =  samplespainterutils::isNearSample(globalContext()->currentProject(), m_clipKey.key, QPoint(x, y), params);
    setIsNearSample(m_currentChannel.has_value());
}

void WaveView::setLastClickPos(const unsigned lastX, const unsigned lastY, const unsigned int x, const unsigned int y)
{
    if (wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key,
                                      m_context->zoom()) != IWavePainter::PlotType::Stem) {
        return;
    }

    const auto currentPosition = QPoint(x, y);
    const auto lastPosition = QPoint(lastX, lastY);

    const auto params = getWavePainterParams();

    if (!m_currentChannel.has_value()) {
        m_currentChannel = samplespainterutils::isNearSample(globalContext()->currentProject(), m_clipKey.key, currentPosition, params);
        return;
    }

    samplespainterutils::setLastClickPos(
        m_currentChannel.value(),
        globalContext()->currentProject(), m_clipKey.key, lastPosition, currentPosition, params);
}
