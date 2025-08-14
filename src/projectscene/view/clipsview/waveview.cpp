/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include <QPainter>
#include <QElapsedTimer>

#include "draw/types/color.h"
#include "global/log.h"

#include "au3/wavepainterutils.h"
#include "au3/samplespainterutils.h"
#include "view/timeline/timelinecontext.h"

using namespace au::projectscene;

static const QColor BACKGROUND_COLOR = QColor(255, 255, 255);
static const QColor SAMPLES_BASE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLES_HIGHLIGHT_COLOR = QColor(255, 255, 255);
static const QColor RMS_BASE_COLOR = QColor(255, 255, 255);
static const QColor CLIPPING_SOLID_COLOR = QColor(239, 71, 111);
static const QColor CENTER_LINE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_HEAD_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_STALK_COLOR = QColor(0, 0, 0);

// AU3 colors from au3/libraries/lib-theme-resources/light/Components/Colors.txt
static const QColor CLASSIC_BACKGROUND_COLOR = QColor(240, 243, 255);               // Unselected: #f0f3ff
static const QColor CLASSIC_BACKGROUND_SELECTED_COLOR = QColor(170, 195, 242);      // Selected: #aac3f2
static const QColor CLASSIC_SAMPLES_BASE_COLOR = QColor(100, 100, 211);             // Sample: #6464D3
static const QColor CLASSIC_SAMPLES_BASE_SELECTED_COLOR = QColor(103, 124, 228);    // SelSample: #677ce4
static const QColor CLASSIC_RMS_COLOR = QColor(151, 151, 253);                      // Rms: #9797FD
static const QColor CLASSIC_CLIPPING_COLOR = QColor(239, 71, 111);                  // Clipped: #ef476f

static const float SAMPLE_HEAD_DEFAULT_ALPHA= 0.6;
static const float SAMPLE_HEAD_CLIP_SELECTED_ALPHA = 0.8;
static const float SAMPLE_HEAD_DATA_SELECTED_ALPHA = 0.9;
static const float SAMPLE_STALK_DEFAULT_ALPHA = 0.4;
static const float SAMPLE_STALK_CLIP_SELECTED_ALPHA = 0.6;
static const float SAMPLE_STALK_DATA_SELECTED_ALPHA = 0.7;

WaveView::WaveView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    //! NOTE: Push history state after edit is completed to avoid multiple unecessary calls.
    connect(this, &WaveView::isIsolationModeChanged, [this]() {
        if (!m_isIsolationMode) {
            pushProjectHistorySampleEdit();
        }
    });

    connect(this, &WaveView::multiSampleEditChanged, [this]() {
        if (!m_multiSampleEdit) {
            pushProjectHistorySampleEdit();
        }
    });

    configuration()->isRMSInWaveformVisibleChanged().onReceive(this, [this](bool) {
        update();
    });

    configuration()->isClippingInWaveformVisibleChanged().onReceive(this, [this](bool) {
        update();
    });
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
    params.showRMS = configuration()->isRMSInWaveformVisible();
    params.showClipping = configuration()->isClippingInWaveformVisible();

    projectscene::ClipStyles::Style clipStyle = configuration()->clipStyle();
    if (clipStyle == projectscene::ClipStyles::Style::COLORFUL) {
        applyColorfulStyle(params, m_clipColor, m_clipSelected);
    } else {
        applyClassicStyle(params, m_clipSelected);
    }

    return params;
}

void WaveView::applyColorfulStyle(IWavePainter::Params& params,
                                  const QColor& clipColor,
                                  bool selected) const
{
    float bgAlpha = selected ? 0.8 : 0.9;
    float normalBgAlpha = 0.8;
    params.style.blankBrush = muse::blendQColors(BACKGROUND_COLOR, clipColor, bgAlpha);
    params.style.normalBackground = muse::blendQColors(BACKGROUND_COLOR, clipColor, normalBgAlpha);
    params.style.selectedBackground = transformColor(params.style.normalBackground);

    params.style.samplePen = muse::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
    params.style.selectedSamplePen = muse::blendQColors(params.style.blankBrush,
                                                        selected ? SAMPLES_HIGHLIGHT_COLOR : SAMPLES_BASE_COLOR,
                                                        0.75);
    params.style.rmsPen = muse::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.25);
    params.style.clippedPen = CLIPPING_SOLID_COLOR;
    params.style.centerLine = muse::blendQColors(params.style.samplePen, CENTER_LINE_COLOR, 0.2);

    float headAlpha = selected ? SAMPLE_HEAD_CLIP_SELECTED_ALPHA : SAMPLE_HEAD_DEFAULT_ALPHA;
    float stalkAlpha = selected ? SAMPLE_STALK_CLIP_SELECTED_ALPHA : SAMPLE_STALK_DEFAULT_ALPHA;

    params.style.sampleHead = muse::blendQColors(params.style.samplePen, SAMPLE_HEAD_COLOR, headAlpha);
    params.style.sampleStalk = muse::blendQColors(params.style.samplePen, SAMPLE_STALK_COLOR, stalkAlpha);

    if (!selected) {
        params.style.sampleHeadSelection = muse::blendQColors(params.style.samplePen, SAMPLE_HEAD_COLOR,
                                                              SAMPLE_HEAD_DATA_SELECTED_ALPHA);
        params.style.sampleStalkSelection
            = muse::blendQColors(params.style.samplePen, SAMPLE_STALK_COLOR, SAMPLE_STALK_DATA_SELECTED_ALPHA);
    }
}

void WaveView::applyClassicStyle(IWavePainter::Params& params, bool selected) const
{
    params.style.blankBrush = selected ? CLASSIC_BACKGROUND_SELECTED_COLOR : CLASSIC_BACKGROUND_COLOR;
    params.style.normalBackground = params.style.blankBrush;
    params.style.selectedBackground = selected ? transformColor(CLASSIC_BACKGROUND_SELECTED_COLOR) : CLASSIC_BACKGROUND_SELECTED_COLOR;

    QColor baseSampleColor = selected ? CLASSIC_SAMPLES_BASE_SELECTED_COLOR : CLASSIC_SAMPLES_BASE_COLOR;
    params.style.samplePen = baseSampleColor;
    params.style.selectedSamplePen = CLASSIC_SAMPLES_BASE_SELECTED_COLOR;
    params.style.rmsPen = CLASSIC_RMS_COLOR;
    params.style.clippedPen = CLASSIC_CLIPPING_COLOR;
    params.style.centerLine = baseSampleColor;
    params.style.sampleHead = baseSampleColor;
    params.style.sampleStalk = baseSampleColor;

    if (!selected) {
        params.style.sampleHeadSelection = baseSampleColor;
        params.style.sampleStalkSelection = baseSampleColor;
    }
}

void WaveView::paint(QPainter* painter)
{
    IWavePainter::Params params = getWavePainterParams();
    IWavePainter::PlotType pType = wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key, params.zoom);

    bool isStemPlot = pType == IWavePainter::PlotType::Stem;

    setIsStemPlot(isStemPlot);
    setAntialiasing(isStemPlot);

    wavePainter()->paint(*painter, m_clipKey.key, params, pType);
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
        connect(m_context, &TimelineContext::zoomChanged, this, &WaveView::onWaveZoomChanged);
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

bool WaveView::isStemPlot() const
{
    return m_isStemPlot;
}

void WaveView::setIsStemPlot(bool isStemPlot)
{
    if (m_isStemPlot == isStemPlot) {
        return;
    }

    m_isStemPlot = isStemPlot;
    emit isStemPlotChanged();
}

int WaveView::currentChannel() const
{
    return m_currentChannel.value_or(0);
}

void WaveView::setCurrentChannel(int currentChannel)
{
    m_currentChannel = currentChannel;
}

bool WaveView::isIsolationMode() const
{
    return m_isIsolationMode;
}

void WaveView::setIsIsolationMode(bool isIsolationMode)
{
    if (m_isIsolationMode == isIsolationMode) {
        return;
    }

    m_isIsolationMode = isIsolationMode;
    emit isIsolationModeChanged();
}

void WaveView::setMultiSampleEdit(bool multiSampleEdit)
{
    if (m_multiSampleEdit == multiSampleEdit) {
        return;
    }

    m_multiSampleEdit = multiSampleEdit;
    emit multiSampleEditChanged();
}

bool WaveView::multiSampleEdit() const
{
    return m_multiSampleEdit;
}

void WaveView::setIsBrush(bool isBrush)
{
    if (m_isBrush == isBrush) {
        return;
    }

    m_isBrush = isBrush;
    emit isBrushChanged();
}

bool WaveView::isBrush() const
{
    return m_isBrush;
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
    m_currentChannel =  samplespainterutils::hitNearestSampleChannelIndex(globalContext()->currentProject(), m_clipKey.key, QPoint(x,
                                                                                                                                   y),
                                                                          params);
    setIsNearSample(m_currentChannel.has_value());
}

void WaveView::setLastClickPos(const unsigned lastX, const unsigned lastY, const unsigned int x, const unsigned int y)
{
    if (wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key,
                                      m_context->zoom()) != IWavePainter::PlotType::Stem) {
        return;
    }

    // Prevent sample editing during playback
    if (playbackState()->isPlaying()) {
        return;
    }

    const auto currentPosition = QPoint(x, y);
    const auto lastPosition = QPoint(lastX, lastY);

    const auto params = getWavePainterParams();

    if (!m_currentChannel.has_value()) {
        m_currentChannel = samplespainterutils::hitNearestSampleChannelIndex(
            globalContext()->currentProject(), m_clipKey.key, currentPosition, params);
        return;
    }

    samplespainterutils::setLastClickPos(
        m_currentChannel.value(),
        globalContext()->currentProject(), m_clipKey.key, lastPosition, currentPosition, params);

    m_lastClickedPoint = currentPosition;
}

void WaveView::smoothLastClickPos(unsigned int x, const unsigned int y)
{
    if (!m_isStemPlot) {
        return;
    }

    // Prevent sample editing during playback
    if (playbackState()->isPlaying()) {
        return;
    }

    const auto currentPosition = QPoint(x, y);
    const auto params = getWavePainterParams();

    auto channel = samplespainterutils::hitChannelIndex(globalContext()->currentProject(), m_clipKey.key, currentPosition, params);

    if (!channel) {
        return;
    }

    samplespainterutils::smoothLastClickPos(
        channel.value(),
        globalContext()->currentProject(), m_clipKey.key, currentPosition, params);

    //! NOTE: History state is only pushed when data is actually changed.
    // For smooth edition there is no data change on button press or release
    // just on mouse click.
    pushProjectHistorySampleEdit();
}

void WaveView::setIsolatedPoint(const unsigned int x, const unsigned int y)
{
    if (!m_isStemPlot) {
        return;
    }

    if (!m_isIsolationMode) {
        return;
    }

    // Prevent sample editing during playback
    if (playbackState()->isPlaying()) {
        return;
    }

    if (!m_lastClickedPoint.has_value()) {
        return;
    }

    const auto currentPosition = QPoint(x, y);
    const auto params = getWavePainterParams();

    if (!m_currentChannel.has_value()) {
        m_currentChannel = samplespainterutils::hitNearestSampleChannelIndex(
            globalContext()->currentProject(), m_clipKey.key, currentPosition, params);
        return;
    }

    samplespainterutils::setIsolatedPoint(
        m_currentChannel.value(),
        m_clipKey.key, globalContext()->currentProject(), m_lastClickedPoint.value(), currentPosition, params);
}

void WaveView::onWaveZoomChanged()
{
    const IWavePainter::PlotType currentPlotType = wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key,
                                                                                 m_context->zoom());
    const bool wasStemPlot = m_isStemPlot;
    const bool isStemPlot = currentPlotType == IWavePainter::PlotType::Stem;

    if (wasStemPlot != isStemPlot) {
        setIsStemPlot(isStemPlot);
        if (!isStemPlot && m_isNearSample) {
            // force isNearSample to false when transitioning away from stem plot mode
            setIsNearSample(false);
        }
        // Note: When transitioning TO stem plot mode, ClipItem.qml onIsStemPlotChanged
        // will trigger mouse position update to force isNearSample to be set correctly
    }

    update();
}

void WaveView::pushProjectHistorySampleEdit()
{
    projectHistory()->pushHistoryState("Moved Samples", "Sample Edit", trackedit::UndoPushType::CONSOLIDATE);
}

au::context::IPlaybackStatePtr WaveView::playbackState() const
{
    return globalContext()->playbackState();
}
