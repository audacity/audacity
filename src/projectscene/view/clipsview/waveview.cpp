/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include <QPainter>
#include <QElapsedTimer>

#include "iwavepainter.h"

#include "../timeline/timelinecontext.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "draw/types/color.h"
#include "connectingdotspainter.h"
#include "log.h"
#include "minmaxrmspainter.h"
#include "samplespainter.h"
#include "samplespainterutils.h"

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

static constexpr auto PIXELS_PER_SAMPLE_WHEN_CONNECTING_POINTS = 0.5;
static constexpr auto PIXELS_PER_SAMPLE_WHEN_INDIVIDUAL_POINTS = 4;

namespace {
au::projectscene::PlotType getPlotType(const au::au3::Au3WaveClip& waveClip, double zoom)
{
    const double sampleRate = waveClip.GetRate();
    const double stretchRatio = waveClip.GetStretchRatio();

    const double rate = sampleRate / stretchRatio;

    const double threshold1 = PIXELS_PER_SAMPLE_WHEN_CONNECTING_POINTS * rate;
    if (zoom < threshold1) {
        return PlotType::MinMaxRMS;
    }

    const double threshold2 = PIXELS_PER_SAMPLE_WHEN_INDIVIDUAL_POINTS * rate;
    if (zoom < threshold2) {
        return PlotType::ConnectingDots;
    }

    return au::projectscene::PlotType::Stem;
}

std::unique_ptr<au::projectscene::IWavePainter> getWavePainter(PlotType plotType)
{
    switch (plotType) {
    case PlotType::MinMaxRMS:
        return std::make_unique<MinMaxRMSPainter>();
    case PlotType::ConnectingDots:
        return std::make_unique<ConnectingDotsPainter>();
    case PlotType::Stem:
        return std::make_unique<SamplesPainter>();
    default:
        break;
    }

    return nullptr;
}
}

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

void WaveView::handleSnapChange(PlotType plotType)
{
    if (plotType == PlotType::Stem) {
        if (!m_snap) {
            m_snap = globalContext()->currentProject()->viewState()->getSnap();
        }
        globalContext()->currentProject()->viewState()->setSnap(Snap { SnapType::Samples, true, false });
    } else {
        globalContext()->currentProject()->viewState()->setSnap(m_snap.value_or(Snap { SnapType::Bar, false, false }));
        m_snap.reset();
    }
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
    au::au3::Au3Project* project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*project, TrackId(m_clipKey.key.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> clip = au::au3::DomAccessor::findWaveClip(track, m_clipKey.key.clipId);
    if (!clip) {
        return;
    }

    painter->setPen(Qt::NoPen);

    const auto params = getWavePainterParams();
    m_currentPlotType = getPlotType(*clip, params.zoom);
    handleSnapChange(m_currentPlotType);

    std::unique_ptr<IWavePainter> wavePainter = getWavePainter(m_currentPlotType);
    if (!wavePainter) {
        return;
    }

    const IWavePainter::Geometry& g = params.geometry;
    const std::vector<double> channelHeight {
        g.height * params.channelHeightRatio,
        g.height * (1 - params.channelHeightRatio),
    };

    WaveMetrics wm;
    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;
    wm.selectionStartTime = 0;
    wm.selectionEndTime = 0;
    wm.width = g.width;
    wm.left = g.left;

    for (size_t channelIndex = 0; channelIndex < clip->NChannels(); ++channelIndex) {
        wm.height = channelHeight[channelIndex];
        wavePainter->paint(channelIndex, *painter, wm, params.style, *track, *clip);
        wm.top += wm.height;
    }
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

bool WaveView::enableMultiSampleEdit() const
{
    return m_enableMultiSampleEdit;
}

void WaveView::setEnableMultiSampleEdit(bool enableMultiSampleEdit)
{
    if (m_enableMultiSampleEdit == enableMultiSampleEdit) {
        return;
    }

    if (!enableMultiSampleEdit) {
        m_lastPosition.reset();
    }

    m_enableMultiSampleEdit = enableMultiSampleEdit;
    emit enableMultiSampleEditChanged();
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
    if (m_currentPlotType != PlotType::Stem) {
        return;
    }

    if (m_enableMultiSampleEdit) {
        setIsNearSample(true);
        return;
    }

    const auto params = getWavePainterParams();

    WaveMetrics wm;
    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;
    wm.selectionStartTime = 0;
    wm.selectionEndTime = 0;

    au::au3::Au3Project* project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*project, TrackId(m_clipKey.key.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> clip = au::au3::DomAccessor::findWaveClip(track, m_clipKey.key.clipId);
    if (!clip) {
        return;
    }
    wm.height = params.geometry.height / clip->NChannels();

    m_currentChannel =  samplespainterutils::isNearSample(*track, *clip, QPoint(x, y), wm);
    setIsNearSample(m_currentChannel.has_value());
}

void WaveView::setLastClickPos(const unsigned int x, const unsigned int y)
{
    if (m_currentPlotType != PlotType::Stem) {
        return;
    }

    au::au3::Au3Project* project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*project, TrackId(m_clipKey.key.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> clip = au::au3::DomAccessor::findWaveClip(track, m_clipKey.key.clipId);
    if (!clip) {
        return;
    }

    const auto params = getWavePainterParams();
    WaveMetrics wm;
    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;
    wm.selectionStartTime = 0;
    wm.selectionEndTime = 0;
    wm.height = params.geometry.height / clip->NChannels();

    auto const currentPosition = QPoint(x, y);

    auto const newChannel = samplespainterutils::isNearSample(*track, *clip, currentPosition, wm);
    if (!m_enableMultiSampleEdit && (m_currentChannel != newChannel)) {
        m_lastPosition.reset();
    }

    if (!m_currentChannel.has_value()) {
        return;
    }

    samplespainterutils::setLastClickPos(
        m_currentChannel.value(), *track, *clip, m_lastPosition, currentPosition, wm, m_enableMultiSampleEdit);
    m_lastPosition = currentPosition;
}
