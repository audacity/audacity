/*
* Audacity: A Digital Audio Editor
*/
#include "waveview.h"

#include "global/translation.h"

#include <QPainter>
#include <QPainterPath>
#include <QTimer>

#include <algorithm>
#include <cmath>

#include "global/types/color.h"
#include "global/log.h"

#include "au3/wavepainterutils.h"
#include "au3/samplespainterutils.h"
#include "view/timeline/timelinecontext.h"
#include "waverenderertoggle.h"

using namespace au::projectscene;

static const QColor BACKGROUND_COLOR = QColor(255, 255, 255);
static const QColor SAMPLES_BASE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLES_HIGHLIGHT_COLOR = QColor(255, 255, 255);
static const QColor RMS_BASE_COLOR = QColor(255, 255, 255);
static const QColor RMS_SELECTED_COLOR = QColor(255, 255, 255); // TODO: This need update
static const QColor CLIPPING_SOLID_COLOR = QColor(239, 71, 111);
static const QColor CENTER_LINE_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_HEAD_COLOR = QColor(0, 0, 0);
static const QColor SAMPLE_STALK_COLOR = QColor(0, 0, 0);

// AU3 colors from au3/libraries/au3-theme-resources/light/Components/Colors.txt
static const QColor CLASSIC_BACKGROUND_COLOR = QColor(240, 243, 255);               // Unselected: #f0f3ff
static const QColor CLASSIC_BACKGROUND_SELECTED_COLOR = QColor(170, 195, 242);      // Selected: #aac3f2
static const QColor CLASSIC_SAMPLES_BASE_COLOR = QColor(100, 100, 211);             // Sample: #6464D3
static const QColor CLASSIC_SAMPLES_BASE_SELECTED_COLOR = QColor(103, 124, 228);    // SelSample: #677ce4
static const QColor CLASSIC_RMS_COLOR = QColor(151, 151, 253);                      // Rms: #9797FD
static const QColor CLASSIC_RMS_SELECTED_COLOR = QColor(151, 151, 253);             // Rms: #9797FD // TODO: This need update
static const QColor CLASSIC_CLIPPING_COLOR = QColor(239, 71, 111);                  // Clipped: #ef476f

static const float SAMPLE_HEAD_DEFAULT_ALPHA= 0.6;
static const float SAMPLE_HEAD_CLIP_SELECTED_ALPHA = 0.8;
static const float SAMPLE_HEAD_DATA_SELECTED_ALPHA = 0.9;
static const float SAMPLE_STALK_DEFAULT_ALPHA = 0.4;
static const float SAMPLE_STALK_CLIP_SELECTED_ALPHA = 0.6;
static const float SAMPLE_STALK_DATA_SELECTED_ALPHA = 0.7;

// ============================================================
// PaintedFallback — internal QQuickPaintedItem child for
// QPainter-based rendering (ConnectingDots, Samples, and
// MinMaxRMS until scene graph path is complete)
// ============================================================
class WaveView::PaintedFallback : public QQuickPaintedItem
{
public:
    explicit PaintedFallback(WaveView* owner)
        : QQuickPaintedItem(owner)
        , m_owner(owner)
    {
    }

    void paint(QPainter* painter) override
    {
        m_owner->paintFallback(painter);
    }

private:
    WaveView* m_owner = nullptr;
};

WaveView::WaveView(QQuickItem* parent)
    : QQuickItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setFlag(QQuickItem::ItemHasContents, true);

    //! NOTE: The fallback starts hidden and is sized/recolored only while in
    //! use (see updatePolish): resizing or recoloring it while hidden queues
    //! texture repaints that Qt then runs alongside the scene graph content.
    m_fallback = new PaintedFallback(this);
    m_fallback->setVisible(false);

    connect(WaveRendererToggle::instance(), &WaveRendererToggle::sceneGraphChanged, this, &WaveView::scheduleRepaint);

    //! NOTE: Push history state after edit is completed to avoid multiple unecessary calls.
    connect(this, &WaveView::isIsolationModeChanged, [this]() {
        if (!m_isIsolationMode) {
            pushProjectHistorySampleEdit();
        }
    });

    connect(this, &WaveView::visibleChanged, [this]() {
        emit isNearSampleChanged();
    });

    connect(this, &WaveView::multiSampleEditChanged, [this]() {
        if (!m_multiSampleEdit) {
            pushProjectHistorySampleEdit();
        }
    });

    configuration()->isRMSInWaveformVisibleChanged().onReceive(this, [this](bool) {
        scheduleRepaint();
    });

    configuration()->isClippingInWaveformVisibleChanged().onReceive(this, [this](bool) {
        scheduleRepaint();
    });

    configuration()->clipStyleChanged().onReceive(this, [this](projectscene::ClipStyles::Style) {
        emit backgroundColorChanged();
        update();
    });
}

WaveView::~WaveView()
{
    storePreparedData();
}

void WaveView::componentComplete()
{
    QQuickItem::componentComplete();

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        scheduleRepaint();
    });
}

void WaveView::storePreparedData()
{
    if (m_clipKey.key.itemId != -1) {
        m_sceneGraph.storeForReuse(m_clipKey.key);
    }
}

void WaveView::scheduleRepaint()
{
    //! NOTE: polish() coalesces all repaint requests of a frame (clip time,
    //! geometry, zoom, selection...) into a single updatePolish() call right
    //! before the scene graph is synced.
    polish();
}

void WaveView::updatePolish()
{
    prepareSceneGraphData();
    updateDrawnSelection();

    if (m_sceneGraph.active()) {
        // Scene graph path — hide fallback, trigger updatePaintNode
        if (m_fallback->isVisible()) {
            m_fallback->setVisible(false);
        }

        if (!m_sceneGraph.nodeUpToDate(static_cast<float>(width()))) {
            update();
        }
    } else {
        // Fallback QPainter path — show fallback, trigger its paint()
        m_fallback->setSize(QSizeF(width(), height()));
        m_fallback->setFillColor(backgroundColor());
        if (!m_fallback->isVisible()) {
            m_fallback->setVisible(true);
        }
        m_fallback->update();

        // The scene graph node would otherwise stay on screen alongside the fallback
        if (m_sceneGraph.hasNode()) {
            update();
        }
    }
}

void WaveView::forceRepaint()
{
    scheduleRepaint();
}

void WaveView::geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry)
{
    QQuickItem::geometryChange(newGeometry, oldGeometry);

    // A view laid out after its properties were set (or resized later) must
    // re-prepare: a preparation that ran at zero height yields no vertices and
    // would leave the view stuck on the fallback renderer.
    if (newGeometry.size() != oldGeometry.size()) {
        scheduleRepaint();
    }
}

void WaveView::itemChange(ItemChange change, const ItemChangeData& value)
{
    QQuickItem::itemChange(change, value);

    // In another window the item gets a new node, whatever the current one shows
    if (change == ItemSceneChange) {
        m_sceneGraph.invalidateNode();
    }
}

void WaveView::setClipKey(const ClipKey& newClipKey)
{
    storePreparedData();

    m_clipKey = newClipKey;
    emit clipKeyChanged();

    // Another clip's vertices must not survive delegate reuse: they would be
    // eligible for the stale-frame retention and the input-signature skip in
    // prepareSceneGraphData().
    m_sceneGraph.reset();

    scheduleRepaint();
}

IWavePainter::Params WaveView::getWavePainterParams() const
{
    IWavePainter::Params params;
    params.geometry.height = height();
    params.geometry.width = width();
    params.geometry.left = 0.0;

    params.zoom = m_context->zoom();
    params.fromTime = (m_clipTime.itemStartTime - m_clipTime.startTime);
    params.toTime = params.fromTime + (m_clipTime.itemEndTime - m_clipTime.itemStartTime);
    params.selectionStartTime = m_clipTime.selectionStartTime;
    params.selectionEndTime = m_clipTime.selectionEndTime;
    params.channelHeightRatio = m_channelHeightRatio;
    params.showRMS = configuration()->isRMSInWaveformVisible();
    params.showClipping = configuration()->isClippingInWaveformVisible();
    params.isLinear = m_isLinear;
    params.dbRange = m_dbRange;
    params.displayBounds = m_displayBounds;

    projectscene::ClipStyles::Style clipStyle = configuration()->clipStyle();
    if (clipStyle == projectscene::ClipStyles::Style::COLORFUL) {
        applyColorfulStyle(params, m_clipColor, m_clipSelectedColor, m_clipSelected);
    } else {
        applyClassicStyle(params, m_clipSelected);
    }

    return params;
}

void WaveView::applyColorfulStyle(IWavePainter::Params& params,
                                  const QColor& clipColor,
                                  const QColor& clipSelectedColor,
                                  bool selected) const
{
    float normalBgAlpha = 0.8;
    if (selected) {
        params.style.blankBrush = clipSelectedColor;
        params.style.normalBackground = clipSelectedColor;
        params.style.selectedBackground = clipSelectedColor;
        params.style.envelopeBackground = clipSelectedColor;
        params.style.selectedEnvelopeBackground = clipSelectedColor;
    } else {
        params.style.blankBrush = muse::blendQColors(BACKGROUND_COLOR, clipColor, 0.9);
        params.style.normalBackground = muse::blendQColors(BACKGROUND_COLOR, clipColor, normalBgAlpha);
        params.style.selectedBackground = clipSelectedColor;
        params.style.envelopeBackground = muse::blendQColors(BACKGROUND_COLOR, clipColor, normalBgAlpha);
        params.style.selectedEnvelopeBackground = clipSelectedColor;
    }

    params.style.samplePen = muse::blendQColors(params.style.blankBrush, SAMPLES_BASE_COLOR, 0.8);
    params.style.selectedSamplePen = muse::blendQColors(params.style.blankBrush,
                                                        selected ? SAMPLES_HIGHLIGHT_COLOR : SAMPLES_BASE_COLOR,
                                                        0.75);
    params.style.rmsPen = muse::blendQColors(params.style.samplePen, RMS_BASE_COLOR, 0.25);
    params.style.rmsSelectedPen = muse::blendQColors(params.style.selectedSamplePen, RMS_BASE_COLOR, 0.6); // TODO: use RMS_SELECTED_COLOR
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

    params.style.envelopeBackground = params.style.blankBrush;
    params.style.selectedEnvelopeBackground
        = selected ? transformColor(CLASSIC_BACKGROUND_SELECTED_COLOR) : CLASSIC_BACKGROUND_SELECTED_COLOR;

    QColor baseSampleColor = selected ? CLASSIC_SAMPLES_BASE_SELECTED_COLOR : CLASSIC_SAMPLES_BASE_COLOR;
    params.style.samplePen = baseSampleColor;
    params.style.selectedSamplePen = CLASSIC_SAMPLES_BASE_SELECTED_COLOR;
    params.style.rmsPen = CLASSIC_RMS_COLOR;
    params.style.rmsSelectedPen = muse::blendQColors(params.style.selectedSamplePen, CLASSIC_RMS_COLOR, 0.6); // TODO: use CLASSIC_RMS_SELECTED_COLOR
    params.style.clippedPen = CLASSIC_CLIPPING_COLOR;
    params.style.centerLine = baseSampleColor;
    params.style.sampleHead = baseSampleColor;
    params.style.sampleStalk = baseSampleColor;

    if (!selected) {
        params.style.sampleHeadSelection = baseSampleColor;
        params.style.sampleStalkSelection = baseSampleColor;
    }
}

void WaveView::paintFallback(QPainter* painter)
{
    if (m_cornerRadius > 0.0) {
        // The same rounded corners as the scene graph path; extending the rect
        // above the item keeps the top corners square when a header covers them
        const qreal radius = std::min({ m_cornerRadius, width() / 2, height() / 2 });
        const qreal top = m_roundTopCorners ? 0.0 : -radius;
        QPainterPath path;
        path.addRoundedRect(QRectF(0.0, top, width(), height() - top), radius, radius);
        painter->setClipPath(path);
    }

    IWavePainter::Params params = getWavePainterParams();
    IWavePainter::PlotType pType = wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key, params.zoom);

    bool isStemPlot = pType == IWavePainter::PlotType::Stem;

    setIsStemPlot(isStemPlot);
    m_fallback->setAntialiasing(isStemPlot);

    wavePainter()->paint(*painter, m_clipKey.key, params, pType);

    if (globalContext()->isRecording()) {
        paintRecordingPlaceholder(*painter, params);
    }
}

void WaveView::paintRecordingPlaceholder(QPainter& painter, const IWavePainter::Params& params)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackedit::Clip clip = prj->clip(m_clipKey.key);
    if (!clip.isValid() || m_clipTime.itemEndTime <= clip.endTime) {
        return;
    }

    const double x0 = std::max(0.0, (clip.endTime - m_clipTime.itemStartTime) * params.zoom);
    const double x1 = std::min(width(), (m_clipTime.itemEndTime - m_clipTime.itemStartTime) * params.zoom);
    if (x1 - x0 < 1.0) {
        return;
    }

    const double bandsHeight = params.geometry.height;
    std::vector<std::pair<double, double> > channelBands;
    if (muse::is_equal(m_channelHeightRatio, 1.0)) {
        channelBands.emplace_back(0.0, bandsHeight);
    } else {
        channelBands.emplace_back(0.0, bandsHeight * m_channelHeightRatio);
        channelBands.emplace_back(bandsHeight * m_channelHeightRatio, bandsHeight * (1.0 - m_channelHeightRatio));
    }

    for (const auto& [top, bandHeight] : channelBands) {
        const double centerY = top + bandHeight / 2.0;

        painter.setPen(params.style.samplePen);
        painter.drawLine(QPointF(x0, centerY), QPointF(x1, centerY));
    }
}

void WaveView::prepareSceneGraphData()
{
    if (!m_context || m_clipKey.key.itemId == -1 || !WaveRendererToggle::instance()->sceneGraph()) {
        m_sceneGraph.reset();
        return;
    }

    WaveSceneGraph::Input input;
    input.params = getWavePainterParams();
    const IWavePainter::PlotType pType = wavepainterutils::getPlotType(
        globalContext()->currentProject(), m_clipKey.key, input.params.zoom);

    if (pType != IWavePainter::PlotType::MinMaxRMS) {
        m_sceneGraph.reset();
        return;
    }

    input.clipData = minMaxRMSPainter()->clipData(m_clipKey.key);
    if (!input.clipData.valid) {
        m_sceneGraph.reset();
        return;
    }

    input.clipKey = m_clipKey.key;
    input.clipTime = m_clipTime;
    input.metrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), m_clipKey.key, input.params);
    input.containerExactX = m_context->timeToPosition(m_clipTime.itemStartTime);
    input.dragGhost = m_dragGhost;
    input.recording = globalContext()->isRecording();
    input.cornerRadius = static_cast<float>(m_cornerRadius);
    input.roundTopCorners = m_roundTopCorners;

    // Incomplete data is completed by the cache on a later lookup: ask again shortly
    // so the view does not stay partial until the next user-triggered repaint
    const bool retry = m_sceneGraph.prepare(input, *minMaxRMSPainter());
    if (retry && !m_retryPending) {
        m_retryPending = true;
        QTimer::singleShot(80, this, [this]() {
            m_retryPending = false;
            scheduleRepaint();
        });
    }
}

QSGNode* WaveView::updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData*)
{
    return m_sceneGraph.updatePaintNode(oldNode, static_cast<float>(width()));
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

        onWaveZoomChanged();
    }

    emit timelineContextChanged();
}

void WaveView::updateView()
{
    scheduleRepaint();
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
    emit backgroundColorChanged();

    scheduleRepaint();
}

QColor WaveView::clipSelectedColor() const
{
    return m_clipSelectedColor;
}

void WaveView::setClipSelectedColor(const QColor& newClipSelectedColor)
{
    if (m_clipSelectedColor == newClipSelectedColor) {
        return;
    }
    m_clipSelectedColor = newClipSelectedColor;
    emit clipSelectedColorChanged();
    emit backgroundColorChanged();

    scheduleRepaint();
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
    emit backgroundColorChanged();

    scheduleRepaint();
}

QColor WaveView::backgroundColor() const
{
    if (!m_context) {
        return QColor();
    }

    const IWavePainter::Params params = getWavePainterParams();

    const bool endWithinSelection = m_clipTime.selectionStartTime < m_clipTime.selectionEndTime
                                    && m_clipTime.selectionStartTime <= m_clipTime.endTime
                                    && m_clipTime.selectionEndTime >= m_clipTime.endTime;

    return endWithinSelection ? params.style.selectedBackground : params.style.normalBackground;
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
    emit backgroundColorChanged();

    scheduleRepaint();
}

double WaveView::channelHeightRatio() const
{
    return m_channelHeightRatio;
}

void WaveView::setChannelHeightRatio(double channelHeightRatio)
{
    if (m_channelHeightRatio == channelHeightRatio) {
        return;
    }

    m_channelHeightRatio = channelHeightRatio;
    emit channelHeightRatioChanged();
    scheduleRepaint();
}

bool WaveView::isNearSample() const
{
    return isVisible() && m_isNearSample;
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

bool WaveView::isLinear() const
{
    return m_isLinear;
}

void WaveView::setIsLinear(bool isLinear)
{
    if (m_isLinear == isLinear) {
        return;
    }

    m_isLinear = isLinear;
    scheduleRepaint();
}

double WaveView::dbRange() const
{
    return m_dbRange;
}

void WaveView::setDbRange(double dbRange)
{
    if (m_dbRange == dbRange) {
        return;
    }

    m_dbRange = dbRange;
    scheduleRepaint();
}

QVariant WaveView::displayBounds() const
{
    QMap<QString, float> bounds;
    bounds["min"] = m_displayBounds.first;
    bounds["max"] = m_displayBounds.second;
    return QVariant::fromValue(bounds);
}

void WaveView::setDisplayBounds(const QVariant& displayBounds)
{
    float minBound = displayBounds.toMap().value("min", -1.0f).toFloat();
    float maxBound = displayBounds.toMap().value("max", 1.0f).toFloat();

    if (m_displayBounds.first == minBound && m_displayBounds.second == maxBound) {
        return;
    }

    m_displayBounds.first = minBound;
    m_displayBounds.second = maxBound;

    scheduleRepaint();
}

qreal WaveView::cornerRadius() const
{
    return m_cornerRadius;
}

void WaveView::setCornerRadius(qreal cornerRadius)
{
    if (m_cornerRadius == cornerRadius) {
        return;
    }

    m_cornerRadius = cornerRadius;
    emit cornerRadiusChanged();
    scheduleRepaint();
}

bool WaveView::roundTopCorners() const
{
    return m_roundTopCorners;
}

void WaveView::setRoundTopCorners(bool roundTopCorners)
{
    if (m_roundTopCorners == roundTopCorners) {
        return;
    }

    m_roundTopCorners = roundTopCorners;
    emit roundTopCornersChanged();
    scheduleRepaint();
}

void WaveView::updateDrawnSelection()
{
    bool valid = false;
    double start = 0.0;
    double end = 0.0;

    const bool hasView = m_context && m_clipKey.key.itemId != -1;
    const IWavePainter::Params params = hasView ? getWavePainterParams() : IWavePainter::Params {};
    if (hasView && params.selectionEndTime > params.selectionStartTime
        && wavepainterutils::getPlotType(globalContext()->currentProject(), m_clipKey.key, params.zoom) == IWavePainter::PlotType::Stem) {
        const IMinMaxRMSPainter::ClipData clipData = minMaxRMSPainter()->clipData(m_clipKey.key);
        if (clipData.valid && clipData.samplesPerSecond > 0.0) {
            // The same snapping and rounding as the samples painter applies to the selected background
            const double samplesPerSecond = clipData.samplesPerSecond;
            const auto toX = [&](double time) {
                const double snapped = std::round((time - clipData.playStartTime) * samplesPerSecond) / samplesPerSecond;
                return std::clamp(std::floor(0.5 + params.zoom * (snapped - params.fromTime)), 0.0, width());
            };
            valid = true;
            start = toX(params.selectionStartTime);
            end = std::max(start, toX(params.selectionEndTime));
        }
    } else if (m_sceneGraph.active()) {
        // The header selection follows the drawn one, which is snapped to the waveform's columns
        if (const auto span = m_sceneGraph.selectionSpan(static_cast<float>(width()))) {
            valid = true;
            start = span->first;
            end = span->second;
        }
    }

    if (valid == m_drawnSelectionValid && start == m_drawnSelectionStart && end == m_drawnSelectionEnd) {
        return;
    }

    m_drawnSelectionValid = valid;
    m_drawnSelectionStart = start;
    m_drawnSelectionEnd = end;
    emit drawnSelectionChanged();
}

bool WaveView::drawnSelectionValid() const
{
    return m_drawnSelectionValid;
}

double WaveView::drawnSelectionStart() const
{
    return m_drawnSelectionStart;
}

double WaveView::drawnSelectionEnd() const
{
    return m_drawnSelectionEnd;
}

bool WaveView::dragGhost() const
{
    return m_dragGhost;
}

void WaveView::setDragGhost(bool dragGhost)
{
    if (m_dragGhost == dragGhost) {
        return;
    }

    m_dragGhost = dragGhost;
    emit dragGhostChanged();
    scheduleRepaint();
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

    scheduleRepaint();
}

void WaveView::pushProjectHistorySampleEdit()
{
    projectHistory()->pushHistoryState(muse::trc("projectscene", "Moved Samples"), muse::trc("projectscene", "Sample Edit"),
                                       trackedit::UndoPushType::CONSOLIDATE);
}

au::context::IPlaybackStatePtr WaveView::playbackState() const
{
    return globalContext()->playbackState();
}
