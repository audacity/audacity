/*
* Audacity: A Digital Audio Editor
*/
#include "wavescenegraph.h"

#include "wavedisplaybounds.h"

#include <QMatrix4x4>
#include <QSGClipNode>
#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGVertexColorMaterial>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstring>
#include <limits>
#include <numbers>
#include <utility>

using namespace au::projectscene;

namespace au::projectscene {
namespace {
//! What a view prepared from a clip, kept while no view shows it
struct PreparedFrame
{
    WaveSceneGraph::InputSignature signature;
    WaveSceneGraph::Frame frame;
};

//! Prepared data left behind by views that went away, so a view recreated for the
//! same clip (e.g. its track scrolled back into view) does not rebuild it. An entry
//! applies only while its signature matches; the data generation in it is unique
//! across clips, so a key reused by another clip or project never matches.
class PreparedDataCache
{
public:
    void put(const trackedit::ClipKey& key, std::shared_ptr<PreparedFrame> data)
    {
        drop(key);
        m_entries.emplace_back(key, std::move(data));
        if (m_entries.size() > MAX_ENTRIES) {
            m_entries.erase(m_entries.begin());
        }
    }

    std::shared_ptr<PreparedFrame> take(const trackedit::ClipKey& key)
    {
        const auto it = find(key);
        if (it == m_entries.end()) {
            return nullptr;
        }
        std::shared_ptr<PreparedFrame> data = std::move(it->second);
        m_entries.erase(it);
        return data;
    }

private:
    static constexpr size_t MAX_ENTRIES = 64;

    using Entries = std::vector<std::pair<trackedit::ClipKey, std::shared_ptr<PreparedFrame> > >;

    Entries::iterator find(const trackedit::ClipKey& key)
    {
        return std::find_if(m_entries.begin(), m_entries.end(), [&key](const auto& entry) {
            return entry.first.trackId == key.trackId && entry.first.itemId == key.itemId;
        });
    }

    void drop(const trackedit::ClipKey& key)
    {
        const auto it = find(key);
        if (it != m_entries.end()) {
            m_entries.erase(it);
        }
    }

    Entries m_entries;
};

//! All views run on the GUI thread
PreparedDataCache& preparedDataCache()
{
    static PreparedDataCache cache;
    return cache;
}

//! How far a rounded corner reaches in from the top or bottom edge at the
//! given distance from the side edge (0 past the corner)
float cornerInset(float distanceToSide, float radius)
{
    if (radius <= 0.0f || distanceToSide >= radius) {
        return 0.0f;
    }
    const float d = radius - std::max(distanceToSide, 0.0f);
    return radius - std::sqrt(std::max(radius * radius - d * d, 0.0f));
}

//! Fills a triangle strip covering [x0, x1] x [0, height] of an item that is
//! itemWidth wide, minus its rounded corners. The corner arcs are approximated
//! by a few vertical slices. xShift is subtracted from the x coordinates written.
void fillRoundedSpan(QSGGeometry* geometry, float x0, float x1, float itemWidth, float height,
                     float radius, bool roundTop, float xShift)
{
    if (x1 <= x0 || height <= 0.0f) {
        geometry->allocate(0);
        return;
    }

    radius = std::min({ radius, itemWidth * 0.5f, height * 0.5f });

    constexpr int ARC_STEPS = 4;
    std::array<float, 2 * ARC_STEPS + 2> xs {};
    int count = 0;
    auto addSlice = [&](float x) {
        if (x > x0 && x < x1) {
            xs[count++] = x;
        }
    };

    xs[count++] = x0;
    for (int i = 1; i <= ARC_STEPS; ++i) {
        addSlice(radius * i / ARC_STEPS);
    }
    for (int i = 0; i < ARC_STEPS; ++i) {
        addSlice(itemWidth - radius + radius * i / ARC_STEPS);
    }
    xs[count++] = x1;

    geometry->allocate(count * 2);
    QSGGeometry::Point2D* vertices = geometry->vertexDataAsPoint2D();
    for (int i = 0; i < count; ++i) {
        const float x = xs[i];
        const float inset = std::max(cornerInset(x, radius), cornerInset(itemWidth - x, radius));
        vertices[2 * i].set(x - xShift, roundTop ? inset : 0.0f);
        vertices[2 * i + 1].set(x - xShift, height - inset);
    }
}

//! Leaves room for the sample heads at the top and bottom of a stem plot channel, as the hit testing expects
constexpr float SAMPLE_HEAD_PADDING = 3.0f;
constexpr float SAMPLE_HEAD_RADIUS = 2.5f;
constexpr float SAMPLE_HEAD_FILL_RADIUS = 1.5f;

using ColoredPoint = WaveSceneGraph::ColoredPoint;

struct Rgba {
    uint8_t r, g, b, a;
};

//! Vertex colors are premultiplied
Rgba toRgba(const QColor& color)
{
    const QRgb rgb = qPremultiply(color.rgba());
    return Rgba { static_cast<uint8_t>(qRed(rgb)), static_cast<uint8_t>(qGreen(rgb)), static_cast<uint8_t>(qBlue(rgb)),
                  static_cast<uint8_t>(qAlpha(rgb)) };
}

void appendTriangle(std::vector<ColoredPoint>& out, float x0, float y0, float x1, float y1, float x2, float y2, Rgba c)
{
    out.push_back({ x0, y0, c.r, c.g, c.b, c.a });
    out.push_back({ x1, y1, c.r, c.g, c.b, c.a });
    out.push_back({ x2, y2, c.r, c.g, c.b, c.a });
}

//! A convex quad given by its corners in order
void appendQuad(std::vector<ColoredPoint>& out, float x0, float y0, float x1, float y1, float x2, float y2, float x3, float y3, Rgba c)
{
    appendTriangle(out, x0, y0, x1, y1, x2, y2, c);
    appendTriangle(out, x0, y0, x2, y2, x3, y3, c);
}

void appendRect(std::vector<ColoredPoint>& out, float left, float top, float right, float bottom, Rgba c)
{
    appendQuad(out, left, top, right, top, right, bottom, left, bottom, c);
}

void appendDisk(std::vector<ColoredPoint>& out, float cx, float cy, float radius, Rgba c)
{
    constexpr int SEGMENTS = 12;
    constexpr float STEP = 2.0f * std::numbers::pi_v<float> / SEGMENTS;
    for (int i = 0; i < SEGMENTS; ++i) {
        appendTriangle(out, cx, cy,
                       cx + radius * std::cos(i * STEP), cy + radius * std::sin(i * STEP),
                       cx + radius * std::cos((i + 1) * STEP), cy + radius * std::sin((i + 1) * STEP), c);
    }
}

void fillTriangles(QSGGeometryNode* node, const std::vector<ColoredPoint>& points)
{
    QSGGeometry* geometry = node->geometry();
    geometry->allocate(static_cast<int>(points.size()));
    QSGGeometry::ColoredPoint2D* vertices = geometry->vertexDataAsColoredPoint2D();
    for (size_t i = 0; i < points.size(); ++i) {
        const ColoredPoint& p = points[i];
        vertices[i].set(p.x, p.y, p.r, p.g, p.b, p.a);
    }
    node->markDirty(QSGNode::DirtyGeometry);
}

void updateClipRect(QSGClipNode* clipNode, float width, float height)
{
    const QRectF rect(0.0, 0.0, width, height);
    QSGGeometry::updateRectGeometry(clipNode->geometry(), rect);
    clipNode->setClipRect(rect);
    clipNode->markDirty(QSGNode::DirtyGeometry);
}
}
}

void WaveSceneGraph::reset()
{
    m_frame.vertices.clear();
    m_frame.sampleTriangles.clear();
    m_frame.sampleHeadTriangles.clear();
    m_active = false;
    m_lastSignatureValid = false;
}

void WaveSceneGraph::storeForReuse(const trackedit::ClipKey& clipKey)
{
    //! NOTE: Only complete data can be reused: its signature is all the next
    //! view checks.
    if (!m_lastSignatureValid || !m_lastComplete || m_frame.empty()) {
        return;
    }

    auto data = std::make_shared<PreparedFrame>();
    data->signature = m_lastSignature;
    data->frame = std::exchange(m_frame, {});
    m_lastSignatureValid = false;

    preparedDataCache().put(clipKey, std::move(data));
}

bool WaveSceneGraph::active() const
{
    return m_active;
}

bool WaveSceneGraph::hasNode() const
{
    return m_nodeValid;
}

bool WaveSceneGraph::nodeUpToDate(float itemWidth) const
{
    return m_nodeValid && !m_dirty && m_appliedContentOffset == m_contentOffset && m_appliedWidth == itemWidth;
}

void WaveSceneGraph::invalidateNode()
{
    m_nodeValid = false;
}

bool WaveSceneGraph::prepare(const Input& input, IMinMaxRMSPainter& painter)
{
    const IWavePainter::Params& params = input.params;
    const Window window = computeWindow(input.clipTime, input.clipData, params.zoom, input.containerExactX, input.dragGhost,
                                        input.recording);

    WaveMetrics metrics = input.metrics;
    metrics.fromTime = window.fromPixelSnapped / params.zoom;
    metrics.toTime = window.toPixelSnapped / params.zoom;
    metrics.left = 0.0;

    const InputSignature sig = makeSignature(input.clipKey, input.clipData, params, metrics, window, input.cornerRadius,
                                             input.roundTopCorners, input.plotType);
    m_cornerRadius = sig.cornerRadius;
    m_roundTopCorners = sig.roundTopCorners;

    if (reusePreparedFrame(input.clipKey, sig, window.contentOffset)) {
        return false;
    }

    Frame previous = std::exchange(m_frame, {});
    const bool prevActive = m_active;
    m_active = false;

    bool complete = true;
    if (input.plotType == IWavePainter::PlotType::MinMaxRMS) {
        complete = buildFrame(input, metrics, window, painter);
    } else {
        buildSampleFrame(input, metrics, window, painter);
    }
    return acceptFrame(std::move(previous), prevActive, input, sig, window, complete);
}

WaveSceneGraph::Window WaveSceneGraph::computeWindow(const ClipTime& clipTime, const IMinMaxRMSPainter::ClipData& clipData, double zoom,
                                                     double containerExactX, bool dragGhost, bool recording)
{
    Window window;

    // The data cache is indexed from the untrimmed sequence start; place the
    // requested window relative to the sequence origin. The origin
    // (playStart - trimLeft) is invariant under trimming, so the window stays
    // consistent with the container position from clipTime even when a
    // repaint runs between the clip trim and the view-state update.
    // A drag ghost is drawn away from the clip, which itself only moves on release
    const double clipStartTime = dragGhost ? clipTime.startTime : clipData.playStartTime;
    const double sequenceStartTime = clipStartTime - clipData.trimLeft;
    const double windowFromTime = clipTime.itemStartTime - sequenceStartTime;
    const double windowToTime = windowFromTime + (clipTime.itemEndTime - clipTime.itemStartTime);

    //! NOTE: Window edges clamped by the viewport are snapped outward to cache
    //! tile boundaries, so while scrolling the prepared window (and with it
    //! the input signature) changes only when a tile boundary is crossed.
    //! Edges that coincide with the clip's own bounds stay exact: they are
    //! already scroll-invariant. The extra columns lie outside the item and
    //! are clipped by the clip item's bounds.
    const double tileWidthPx = clipData.tileWidth;
    window.fromPixelExact = windowFromTime * zoom;
    window.fromPixelSnapped = window.fromPixelExact;
    if (clipTime.itemStartTime > clipTime.startTime) {
        window.fromPixelSnapped = std::floor(window.fromPixelExact / tileWidthPx) * tileWidthPx;
    }

    window.toPixelExact = windowToTime * zoom;
    window.toPixelSnapped = window.toPixelExact;
    if (clipTime.itemEndTime < clipTime.endTime) {
        // Snapping must not request columns past the sequence end: the
        // trailing cache element would never complete and keep retries alive.
        const double sequenceEndPixel = (clipData.sequenceEndTime - sequenceStartTime) * zoom;
        window.toPixelSnapped = std::min(std::ceil(window.toPixelExact / tileWidthPx) * tileWidthPx,
                                         std::max(window.toPixelExact, std::floor(sequenceEndPixel)));
    }

    //! NOTE While recording, the clip reaches ahead of the recorded audio: the audio
    //! is looked up only up to its end, and the rest is drawn as a flat placeholder line
    const double dataEndPixel = (clipData.playEndTime - sequenceStartTime) * zoom;
    window.hasPlaceholder = recording && !dragGhost && window.toPixelExact - dataEndPixel >= 1.0;
    window.placeholderFromPixel = std::max(window.fromPixelSnapped, dataEndPixel);
    if (window.hasPlaceholder) {
        window.toPixelSnapped = std::min(window.toPixelSnapped, window.placeholderFromPixel);
    }

    // The clip container is positioned at integer x (see TrackClipsListModel);
    // the vertex data starts at the snapped window origin. The content node
    // transform carries the snap correction plus the sub-pixel residual, so
    // the waveform stays anchored to the timeline rather than to the rounded
    // container, and scrolling moves existing vertices instead of building
    // new ones. A drag ghost moves as a whole, so its waveform follows the
    // rounded container instead.
    window.subpixelResidual = dragGhost ? 0.0 : containerExactX - std::floor(0.5 + containerExactX);
    window.contentOffset = window.subpixelResidual - (window.fromPixelExact - window.fromPixelSnapped);

    return window;
}

WaveSceneGraph::InputSignature WaveSceneGraph::makeSignature(const trackedit::ClipKey& clipKey, const IMinMaxRMSPainter::ClipData& clipData,
                                                             const IWavePainter::Params& params, const WaveMetrics& metrics,
                                                             const Window& window, float cornerRadius, bool roundTopCorners,
                                                             IWavePainter::PlotType plotType)
{
    InputSignature sig;
    sig.trackId = clipKey.trackId;
    sig.itemId = clipKey.itemId;
    sig.dataVersion = clipData.dataVersion;
    sig.plotType = plotType;
    sig.channelCount = clipData.channelCount;
    sig.fromTime = metrics.fromTime;
    sig.toTime = metrics.toTime;
    sig.placeholderToTime = window.hasPlaceholder ? window.toPixelExact / params.zoom : 0.0;
    sig.zoom = params.zoom;
    sig.height = params.geometry.height;
    sig.channelHeightRatio = params.channelHeightRatio;
    sig.selectionStartTime = metrics.selectionStartTime;
    sig.selectionEndTime = metrics.selectionEndTime;
    sig.dbRange = params.dbRange;
    sig.displayMin = params.displayBounds.first;
    sig.displayMax = params.displayBounds.second;
    sig.showRMS = params.showRMS;
    sig.showClipping = params.showClipping;
    sig.isLinear = params.isLinear;
    sig.cornerRadius = cornerRadius;
    sig.roundTopCorners = roundTopCorners;
    sig.background = params.style.normalBackground.rgba();
    sig.selectedBackground = params.style.selectedBackground.rgba();
    sig.centerLine = params.style.centerLine.rgba();
    sig.samplePen = params.style.samplePen.rgba();
    sig.selectedSamplePen = params.style.selectedSamplePen.rgba();
    sig.rmsPen = params.style.rmsPen.rgba();
    sig.rmsSelectedPen = params.style.rmsSelectedPen.rgba();
    sig.clippedPen = params.style.clippedPen.rgba();
    sig.sampleBrush = params.style.sampleBrush.rgba();
    sig.sampleHead = params.style.sampleHead.rgba();
    sig.sampleHeadSelection = params.style.sampleHeadSelection.rgba();
    sig.sampleStalk = params.style.sampleStalk.rgba();
    return sig;
}

bool WaveSceneGraph::reusePreparedFrame(const trackedit::ClipKey& clipKey, const InputSignature& sig, double contentOffset)
{
    //! NOTE: Skips straight to the transform update when nothing the vertices
    //! depend on has changed.
    const bool canSkip = m_lastSignatureValid && m_lastComplete
                         && m_staleRetainCount == 0
                         && !m_frame.empty() && sig == m_lastSignature;
    if (canSkip) {
        m_contentOffset = contentOffset;
        m_active = true;
        return true;
    }

    // A view created for this clip again starts empty: take over what the
    // previous one prepared, if it was prepared from the same inputs
    if (!m_frame.empty()) {
        return false;
    }

    const std::shared_ptr<PreparedFrame> prepared = preparedDataCache().take(clipKey);
    if (!prepared || !(prepared->signature == sig)) {
        return false;
    }

    m_frame = std::move(prepared->frame);
    m_contentOffset = contentOffset;
    m_lastSignature = sig;
    m_lastSignatureValid = true;
    m_lastComplete = true;
    m_staleRetainCount = 0;
    m_active = true;
    m_dirty = true;
    return true;
}

WaveSceneGraph::ColumnColors WaveSceneGraph::columnColors(const IWavePainter::Style& style)
{
    auto toRgb = [](const QColor& color) {
        const QRgb rgb = color.rgb();
        return Rgb { static_cast<uint8_t>(qRed(rgb)), static_cast<uint8_t>(qGreen(rgb)), static_cast<uint8_t>(qBlue(rgb)) };
    };
    return ColumnColors {
        toRgb(style.samplePen),
        toRgb(style.selectedSamplePen),
        toRgb(style.rmsPen),
        toRgb(style.rmsSelectedPen),
        toRgb(style.clippedPen),
    };
}

WaveSceneGraph::Vertex WaveSceneGraph::toVertex(const WaveColumnVertex& col, float channelTop, float channelBottom,
                                                const ColumnColors& colors)
{
    Vertex v;
    v.x = col.x;

    if (col.clipping) {
        // Full-height clipping bar matches bitmap renderer's behavior
        v.maxY = channelTop;
        v.minY = channelBottom;
        v.rmsMaxY = channelTop;
        v.rmsMinY = channelBottom;

        v.r = colors.clipping.r;
        v.g = colors.clipping.g;
        v.b = colors.clipping.b;
        v.a = 255;

        v.rmsR = colors.clipping.r;
        v.rmsG = colors.clipping.g;
        v.rmsB = colors.clipping.b;
        v.rmsA = 255;
        return v;
    }

    v.maxY = std::clamp(channelTop + col.maxY, channelTop, channelBottom);
    v.minY = std::clamp(channelTop + col.minY, channelTop, channelBottom);
    v.rmsMaxY = std::clamp(channelTop + col.rmsMaxY, channelTop, channelBottom);
    v.rmsMinY = std::clamp(channelTop + col.rmsMinY, channelTop, channelBottom);

    const Rgb& sc = col.selected ? colors.selectedSample : colors.sample;
    v.r = sc.r;
    v.g = sc.g;
    v.b = sc.b;
    v.a = 255;

    const Rgb& rc = col.selected ? colors.selectedRms : colors.rms;
    v.rmsR = rc.r;
    v.rmsG = rc.g;
    v.rmsB = rc.b;
    v.rmsA = 255;
    return v;
}

void WaveSceneGraph::clampToCorners(Vertex& v, float inset, bool roundTop, float viewHeight)
{
    if (inset <= 0.0f) {
        return;
    }
    const float top = roundTop ? inset : 0.0f;
    const float bottom = viewHeight - inset;
    v.maxY = std::clamp(v.maxY, top, bottom);
    v.minY = std::clamp(v.minY, v.maxY, bottom);
    v.rmsMaxY = std::clamp(v.rmsMaxY, top, bottom);
    v.rmsMinY = std::clamp(v.rmsMinY, v.rmsMaxY, bottom);
}

bool WaveSceneGraph::buildFrame(const Input& input, WaveMetrics waveMetrics, const Window& window, IMinMaxRMSPainter& painter)
{
    const IWavePainter::Params& params = input.params;
    const std::array<double, 2> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    m_frame.backgroundColor = params.style.normalBackground;
    m_frame.selectedBackgroundColor = params.style.selectedBackground;
    m_frame.zeroLineColor = params.style.centerLine;
    m_frame.showRMS = params.showRMS;
    m_frame.height = static_cast<float>(params.geometry.height);

    // Converted once rather than per column
    const ColumnColors colors = columnColors(params.style);

    // Track selection pixel range for overlay rect
    float selMinX = std::numeric_limits<float>::max();
    float selMaxX = std::numeric_limits<float>::lowest();

    bool allDataComplete = true;

    const size_t channelCount = std::min<size_t>(input.clipData.channelCount, channelHeight.size());

    // One column per pixel of the window and channel, plus one for the partial pixel at each end
    const double windowColumns = std::ceil((waveMetrics.toTime - waveMetrics.fromTime) * params.zoom);
    if (windowColumns > 0.0) {
        m_frame.vertices.reserve(channelCount * (static_cast<size_t>(windowColumns) + 2));
    }

    //! NOTE: All views run on the GUI thread; sharing the buffer keeps its
    //! capacity between calls without each view holding a copy.
    static std::vector<WaveColumnVertex> columnData;

    // Columns stay inside the rounded corners of the clip's real edges (in item
    // space; an edge clamped by the viewport is far outside the item). The
    // edges follow the clip on screen, so scrolling does not invalidate this.
    const float viewHeight = static_cast<float>(params.geometry.height);
    const float cornerRadius = std::min(m_cornerRadius, viewHeight * 0.5f);
    const double clipLeftEdge = (input.clipTime.startTime - input.clipTime.itemStartTime) * params.zoom + window.subpixelResidual;
    const double clipRightEdge = (input.clipTime.endTime - input.clipTime.itemStartTime) * params.zoom + window.subpixelResidual;
    auto columnCornerInset = [&](float contentX) {
        if (cornerRadius <= 0.0f) {
            return 0.0f;
        }
        const double center = contentX + window.contentOffset + 0.5;
        return std::max(cornerInset(static_cast<float>(center - clipLeftEdge), cornerRadius),
                        cornerInset(static_cast<float>(clipRightEdge - center), cornerRadius));
    };

    double topOffset = 0.0;
    for (size_t ch = 0; ch < channelCount; ++ch) {
        waveMetrics.height = channelHeight[ch];
        waveMetrics.top = topOffset;

        const bool channelDataComplete = painter.columns(input.clipKey, ch, params, waveMetrics, columnData);
        allDataComplete = allDataComplete && channelDataComplete;

        const auto [displayMin, displayMax] = waveDisplayBounds(params);
        const float lastRow = static_cast<float>(channelHeight[ch]) - 1;
        const bool zeroLineVisible = displayMin <= 0.0f && displayMax >= 0.0f && displayMax > displayMin;
        const float zeroLineY = displayMax > displayMin ? std::clamp(displayMax / (displayMax - displayMin) * lastRow, 0.0f, lastRow)
                                : lastRow * 0.5f;

        if (window.hasPlaceholder) {
            for (const double pixel : { window.placeholderFromPixel, window.toPixelExact }) {
                WaveColumnVertex column {};
                column.x = static_cast<float>(pixel - window.fromPixelSnapped);
                column.maxY = zeroLineY - 0.5f;
                column.minY = zeroLineY + 0.5f;
                column.rmsMaxY = zeroLineY;
                column.rmsMinY = zeroLineY;
                columnData.push_back(column);
            }
        }

        if (ch > 0) {
            m_frame.channelSplitIndex = m_frame.vertices.size();
        }

        const float channelTop = static_cast<float>(topOffset);
        const float channelBottom = channelTop + static_cast<float>(channelHeight[ch]);

        for (const auto& col : columnData) {
            Vertex v = toVertex(col, channelTop, channelBottom, colors);
            clampToCorners(v, columnCornerInset(col.x), m_roundTopCorners, viewHeight);

            if (col.selected && ch == 0) {
                selMinX = std::min(selMinX, col.x);
                selMaxX = std::max(selMaxX, col.x + 1.0f);
                m_frame.hasSelection = true;
            }

            m_frame.vertices.push_back(v);
        }

        if (zeroLineVisible) {
            m_frame.zeroLineYs.push_back(channelTop + zeroLineY);
        }

        topOffset += channelHeight[ch];
    }

    if (m_frame.hasSelection) {
        m_frame.selectionLeft = selMinX;
        m_frame.selectionRight = selMaxX;
    }

    return allDataComplete;
}

void WaveSceneGraph::buildSampleFrame(const Input& input, WaveMetrics waveMetrics, const Window& window, IMinMaxRMSPainter& painter)
{
    const IWavePainter::Params& params = input.params;
    const std::array<double, 2> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    m_frame.backgroundColor = params.style.normalBackground;
    m_frame.selectedBackgroundColor = params.style.selectedBackground;
    m_frame.zeroLineColor = params.style.centerLine;
    m_frame.height = static_cast<float>(params.geometry.height);

    if (waveMetrics.selectionEndTime > waveMetrics.selectionStartTime) {
        m_frame.hasSelection = true;
        m_frame.selectionLeft = static_cast<float>((waveMetrics.selectionStartTime - waveMetrics.fromTime) * params.zoom);
        m_frame.selectionRight = static_cast<float>((waveMetrics.selectionEndTime - waveMetrics.fromTime) * params.zoom);
    }

    const bool stems = input.plotType == IWavePainter::PlotType::Stem;
    const float padding = stems ? SAMPLE_HEAD_PADDING : 0.0f;

    const float viewHeight = static_cast<float>(params.geometry.height);
    const float cornerRadius = std::min(m_cornerRadius, viewHeight * 0.5f);
    const double clipLeftEdge = (input.clipTime.startTime - input.clipTime.itemStartTime) * params.zoom + window.subpixelResidual;
    const double clipRightEdge = (input.clipTime.endTime - input.clipTime.itemStartTime) * params.zoom + window.subpixelResidual;

    //! NOTE: All views run on the GUI thread; sharing the buffer keeps its capacity
    static std::vector<WaveSamplePoint> samples;

    const size_t channelCount = std::min<size_t>(input.clipData.channelCount, channelHeight.size());
    double topOffset = 0.0;
    for (size_t ch = 0; ch < channelCount; ++ch) {
        const float channelTop = static_cast<float>(topOffset) + padding;
        const float height = std::max(static_cast<float>(channelHeight[ch]) - 2.0f * padding, 0.0f);
        waveMetrics.top = channelTop;
        waveMetrics.height = height;

        painter.samples(input.clipKey, ch, params, waveMetrics, samples);

        // Samples stay inside the rounded corners of the clip's real edges, like the columns
        if (cornerRadius > 0.0f) {
            for (WaveSamplePoint& sample : samples) {
                const double center = sample.x + window.contentOffset + 0.5;
                const float inset = std::max(cornerInset(static_cast<float>(center - clipLeftEdge), cornerRadius),
                                             cornerInset(static_cast<float>(clipRightEdge - center), cornerRadius));
                if (inset > 0.0f) {
                    const float top = (m_roundTopCorners ? inset : 0.0f) + 0.5f - channelTop;
                    const float bottom = viewHeight - inset - 0.5f - channelTop;
                    sample.y = std::clamp(sample.y, top, std::max(top, bottom));
                }
            }
        }

        const auto [displayMin, displayMax] = waveDisplayBounds(params);
        const bool zeroLineVisible = displayMin <= 0.0f && displayMax >= 0.0f && displayMax > displayMin;
        const float zeroLineY = channelTop + (displayMax == displayMin ? height * 0.5f
                                              : std::clamp(displayMax / (displayMax - displayMin) * (height - 1.0f), -1.0f, height));

        if (stems) {
            appendStems(m_frame, samples, channelTop, height, zeroLineY, params.style, params.showClipping);
        } else {
            appendConnectedDots(m_frame, samples, channelTop, height, params.style, params.showClipping);
        }

        if (window.hasPlaceholder) {
            const auto from = static_cast<float>(window.placeholderFromPixel - window.fromPixelSnapped);
            const auto to = static_cast<float>(window.toPixelExact - window.fromPixelSnapped);
            appendRect(m_frame.sampleTriangles, from, zeroLineY - 0.5f, to, zeroLineY + 0.5f, toRgba(params.style.samplePen));
        }

        if (zeroLineVisible) {
            m_frame.zeroLineYs.push_back(zeroLineY);
        }
        topOffset += channelHeight[ch];
    }
}

void WaveSceneGraph::appendConnectedDots(Frame& frame, const std::vector<WaveSamplePoint>& samples, float channelTop,
                                         float channelHeight, const IWavePainter::Style& style, bool showClipping)
{
    const Rgba line = toRgba(style.samplePen);
    const Rgba selectedLine = toRgba(style.selectedSamplePen);
    const Rgba clipped = toRgba(style.clippedPen);
    auto& out = frame.sampleTriangles;

    if (showClipping) {
        for (const WaveSamplePoint& sample : samples) {
            if (sample.clipping) {
                appendRect(out, sample.x - 0.5f, channelTop, sample.x + 0.5f, channelTop + channelHeight, clipped);
            }
        }
    }

    // A segment between two samples is a band one pixel thick along its shorter
    // extent, centred on them like the columns' band, so that it covers one pixel
    // per row or column, as a line without antialiasing would
    for (size_t i = 0; i + 1 < samples.size(); ++i) {
        const WaveSamplePoint& a = samples[i];
        const WaveSamplePoint& b = samples[i + 1];
        const Rgba color = a.selected && b.selected ? selectedLine : line;
        const float ya = channelTop + a.y;
        const float yb = channelTop + b.y;
        if (std::abs(b.x - a.x) >= std::abs(yb - ya)) {
            appendQuad(out, a.x, ya - 0.5f, b.x, yb - 0.5f, b.x, yb + 0.5f, a.x, ya + 0.5f, color);
        } else {
            appendQuad(out, a.x - 0.5f, ya, a.x + 0.5f, ya, b.x + 0.5f, yb, b.x - 0.5f, yb, color);
        }
    }

    for (const WaveSamplePoint& sample : samples) {
        const float y = channelTop + sample.y;
        appendRect(out, sample.x - 0.5f, y - 0.5f, sample.x + 0.5f, y + 0.5f, sample.selected ? selectedLine : line);
    }
}

void WaveSceneGraph::appendStems(Frame& frame, const std::vector<WaveSamplePoint>& samples, float channelTop, float channelHeight,
                                 float zeroLineY, const IWavePainter::Style& style, bool showClipping)
{
    const Rgba stalk = toRgba(style.sampleStalk);
    const Rgba head = toRgba(style.sampleHead);
    const Rgba selectedHead = toRgba(style.sampleHeadSelection);
    const Rgba fill = toRgba(style.sampleBrush);
    const Rgba clipped = toRgba(style.clippedPen);

    for (const WaveSamplePoint& sample : samples) {
        const bool clipping = showClipping && sample.clipping;
        const float y = channelTop + sample.y;
        appendRect(frame.sampleTriangles, sample.x - 0.5f, std::min(y, zeroLineY) - 0.5f, sample.x + 0.5f, std::max(y, zeroLineY) + 0.5f,
                   clipping ? clipped : stalk);

        if (sample.y < 0.0f || sample.y >= channelHeight) {
            continue;
        }

        appendDisk(frame.sampleHeadTriangles, sample.x, y, SAMPLE_HEAD_RADIUS,
                   sample.selected ? selectedHead : (clipping ? clipped : head));
        appendDisk(frame.sampleHeadTriangles, sample.x, y, SAMPLE_HEAD_FILL_RADIUS, clipping ? clipped : fill);
    }
}

bool WaveSceneGraph::acceptFrame(Frame previous, bool prevActive, const Input& input, const InputSignature& sig, const Window& window,
                                 bool complete)
{
    const IWavePainter::Params& params = input.params;

    //! NOTE: Mid-gesture, the clip window and the zoom can update out of sync
    //! for one frame, which yields an empty column set here. Keep the previous
    //! frame's geometry rather than flashing the QPainter fallback: the retry
    //! below delivers the consistent data. A second consecutive empty frame
    //! falls through to the fallback, so a genuinely empty view cannot retain
    //! stale content indefinitely.
    // A clip outside the view has an empty window: there is nothing to retain for
    const bool geometryValid = params.geometry.width > 0.0 && params.geometry.height > 0.0
                               && input.clipTime.itemEndTime > input.clipTime.itemStartTime;
    const bool retainedStale = m_frame.empty() && prevActive && !previous.empty()
                               && m_staleRetainCount < 1 && geometryValid;
    if (retainedStale) {
        m_frame.vertices = std::move(previous.vertices);
        m_frame.sampleTriangles = std::move(previous.sampleTriangles);
        m_frame.sampleHeadTriangles = std::move(previous.sampleHeadTriangles);
        m_frame.channelSplitIndex = previous.channelSplitIndex;
        m_frame.hasSelection = previous.hasSelection;
        m_frame.selectionLeft = previous.selectionLeft;
        m_frame.selectionRight = previous.selectionRight;
        ++m_staleRetainCount;
    } else if (!m_frame.empty()) {
        m_staleRetainCount = 0;
    }

    // Incomplete cache elements are recomputed by the cache on the next lookup;
    // ask again shortly so the snapshot does not stay partial until the next
    // user-triggered repaint. Only retry while retries make progress, otherwise
    // a clip whose data cannot complete would retry forever.
    const bool madeProgress = !retainedStale
                              && (m_frame.vertices.size() != previous.vertices.size()
                                  || (!m_frame.vertices.empty()
                                      && std::memcmp(m_frame.vertices.data(), previous.vertices.data(),
                                                     m_frame.vertices.size() * sizeof(Vertex)) != 0));
    const bool retry = retainedStale || (!complete && madeProgress);

    // A retained frame keeps the previous offset along with the previous
    // vertices: its own window may be the inconsistent one.
    if (!retainedStale) {
        m_contentOffset = window.contentOffset;
    }
    m_lastSignature = sig;
    m_lastSignatureValid = !retainedStale;
    m_lastComplete = complete;

    m_active = !m_frame.empty();
    m_dirty = true;

    return retry;
}

std::optional<std::pair<float, float> > WaveSceneGraph::selectionSpan(float itemWidth) const
{
    if (!m_frame.hasSelection) {
        return std::nullopt;
    }

    // The selection is in content space
    const float offset = static_cast<float>(m_contentOffset);
    const float left = std::max(m_frame.selectionLeft + offset, 0.0f);
    const float right = std::min(m_frame.selectionRight + offset, itemWidth);
    return std::make_pair(left, right);
}

void WaveSceneGraph::fillSelectionGeometry(QSGGeometry* geometry, float itemWidth) const
{
    const auto span = selectionSpan(itemWidth);
    if (!span) {
        geometry->allocate(0);
        return;
    }

    // The corners are in item space
    fillRoundedSpan(geometry, span->first, span->second, itemWidth, m_frame.height, m_cornerRadius, m_roundTopCorners,
                    static_cast<float>(m_contentOffset));
}

QSGNode* WaveSceneGraph::updatePaintNode(QSGNode* oldNode, float itemWidth)
{
    if (!m_active) {
        delete oldNode;
        m_nodeValid = false;
        return nullptr;
    }

    // Node structure (fixed order, child index matters for pointer recovery):
    //   root (QSGNode)
    //   ├── [0] background       (flat color rect, item space)
    //   ├── [1] content          (QSGTransformNode: snap correction + sub-pixel offset)
    //   │       ├── [0] selection bg  (flat color rect, empty when no selection)
    //   │       ├── [1] waveform body (vertex-colored triangle strip)
    //   │       ├── [2] rms band      (vertex-colored triangle strip, empty when showRMS=false)
    //   │       └── [3] samples       (vertex-colored triangles: sample lines and stalks)
    //   ├── [2] zero line        (flat color rect, item space)
    //   └── [3] foreground       (QSGTransformNode: as content)
    //           └── [0] sample heads  (vertex-colored triangles)

    constexpr int IDX_BG = 0;
    constexpr int IDX_CONTENT = 1;
    constexpr int IDX_ZERO = 2;
    constexpr int IDX_FOREGROUND = 3;
    constexpr int IDX_SEL_BG = 0;
    constexpr int IDX_WAVE = 1;
    constexpr int IDX_RMS = 2;
    constexpr int IDX_SAMPLES = 3;
    constexpr int IDX_SAMPLE_HEADS = 0;

    QSGNode* root = oldNode;

    // A repaint whose inputs matched the previous preparation only moves the
    // existing content: refresh the transform, track the live item width with
    // the item-space quads (it changes while scrolling viewport-clamped clips
    // and is deliberately not part of the input signature), and leave the
    // wave geometry alone.
    auto applyContentOffset = [this](QSGNode* node) {
        QMatrix4x4 matrix;
        matrix.translate(static_cast<float>(m_contentOffset), 0.0f);
        auto* transformNode = static_cast<QSGTransformNode*>(node);
        transformNode->setMatrix(matrix);
        transformNode->markDirty(QSGNode::DirtyMatrix);
    };

    if (!m_dirty && root && root->childCount() == 4) {
        // Only what changed is marked dirty
        const bool offsetChanged = m_appliedContentOffset != m_contentOffset;
        if (offsetChanged) {
            applyContentOffset(root->childAtIndex(IDX_CONTENT));
            applyContentOffset(root->childAtIndex(IDX_FOREGROUND));
            m_appliedContentOffset = m_contentOffset;
        }

        const float w = itemWidth;
        const bool widthChanged = m_appliedWidth != w;

        // The selection is clipped by the item's corners, which move against the content
        if ((offsetChanged || widthChanged) && m_frame.hasSelection) {
            auto* contentNode = root->childAtIndex(IDX_CONTENT);
            auto* selBgNode = static_cast<QSGGeometryNode*>(contentNode->childAtIndex(IDX_SEL_BG));
            fillSelectionGeometry(selBgNode->geometry(), w);
            selBgNode->markDirty(QSGNode::DirtyGeometry);
        }

        if (widthChanged) {
            auto* bgNode = static_cast<QSGGeometryNode*>(root->childAtIndex(IDX_BG));
            fillRoundedSpan(bgNode->geometry(), 0.0f, w, w, m_frame.height, m_cornerRadius, m_roundTopCorners, 0.0f);
            bgNode->markDirty(QSGNode::DirtyGeometry);

            auto* zeroLineNode = static_cast<QSGGeometryNode*>(root->childAtIndex(IDX_ZERO));
            auto* zv = zeroLineNode->geometry()->vertexDataAsPoint2D();
            int zi = 0;
            for (float y : m_frame.zeroLineYs) {
                zv[zi++].set(0, y - 0.5f);
                zv[zi++].set(w, y - 0.5f);
                zv[zi++].set(0, y + 0.5f);
                zv[zi++].set(w, y - 0.5f);
                zv[zi++].set(w, y + 0.5f);
                zv[zi++].set(0, y + 0.5f);
            }
            zeroLineNode->markDirty(QSGNode::DirtyGeometry);

            updateClipRect(static_cast<QSGClipNode*>(root), w, m_frame.height);
            m_appliedWidth = w;
        }

        m_nodeValid = true;

        return root;
    }

    auto makeFlatNode = [](int vertexCount) {
        auto* node = new QSGGeometryNode();
        auto* geo = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), vertexCount);
        geo->setDrawingMode(QSGGeometry::DrawTriangleStrip);
        node->setGeometry(geo);
        node->setFlag(QSGNode::OwnsGeometry);
        auto* mat = new QSGFlatColorMaterial();
        node->setMaterial(mat);
        node->setFlag(QSGNode::OwnsMaterial);
        return node;
    };

    auto makeColoredNode = []() {
        auto* node = new QSGGeometryNode();
        auto* geo = new QSGGeometry(QSGGeometry::defaultAttributes_ColoredPoint2D(), 0);
        geo->setDrawingMode(QSGGeometry::DrawTriangleStrip);
        node->setGeometry(geo);
        node->setFlag(QSGNode::OwnsGeometry);
        auto* mat = new QSGVertexColorMaterial();
        node->setMaterial(mat);
        node->setFlag(QSGNode::OwnsMaterial);
        return node;
    };

    auto makeColoredTrianglesNode = [&makeColoredNode]() {
        auto* node = makeColoredNode();
        node->geometry()->setDrawingMode(QSGGeometry::DrawTriangles);
        return node;
    };

    if (!root) {
        //! NOTE: The content may extend past the item: the window is snapped out
        //! to whole cache tiles at viewport-clamped edges. The rectangular clip
        //! (a scissor) keeps it inside, as the clip's layer used to.
        auto* clipRoot = new QSGClipNode();
        clipRoot->setIsRectangular(true);
        clipRoot->setGeometry(new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), 4));
        clipRoot->setFlag(QSGNode::OwnsGeometry);
        root = clipRoot;
        root->appendChildNode(makeFlatNode(4));          // background
        auto* content = new QSGTransformNode();
        content->appendChildNode(makeFlatNode(4));       // selection background
        content->appendChildNode(makeColoredNode());     // waveform
        content->appendChildNode(makeColoredNode());     // rms
        content->appendChildNode(makeColoredTrianglesNode()); // samples
        root->appendChildNode(content);                  // content
        root->appendChildNode(makeFlatNode(4));          // zero line
        auto* foreground = new QSGTransformNode();
        foreground->appendChildNode(makeColoredTrianglesNode()); // sample heads
        root->appendChildNode(foreground);               // foreground
    }

    auto* bgNode = static_cast<QSGGeometryNode*>(root->childAtIndex(IDX_BG));
    auto* contentNode = static_cast<QSGTransformNode*>(root->childAtIndex(IDX_CONTENT));
    auto* zeroLineNode = static_cast<QSGGeometryNode*>(root->childAtIndex(IDX_ZERO));
    auto* selBgNode = static_cast<QSGGeometryNode*>(contentNode->childAtIndex(IDX_SEL_BG));
    auto* waveformNode = static_cast<QSGGeometryNode*>(contentNode->childAtIndex(IDX_WAVE));
    auto* rmsNode = static_cast<QSGGeometryNode*>(contentNode->childAtIndex(IDX_RMS));
    auto* samplesNode = static_cast<QSGGeometryNode*>(contentNode->childAtIndex(IDX_SAMPLES));
    auto* sampleHeadsNode = static_cast<QSGGeometryNode*>(root->childAtIndex(IDX_FOREGROUND)->childAtIndex(IDX_SAMPLE_HEADS));

    applyContentOffset(contentNode);
    applyContentOffset(root->childAtIndex(IDX_FOREGROUND));

    const int N = static_cast<int>(m_frame.vertices.size());
    const float w = itemWidth;

    // Background (full clip area, minus the rounded corners)
    {
        fillRoundedSpan(bgNode->geometry(), 0.0f, w, w, m_frame.height, m_cornerRadius, m_roundTopCorners, 0.0f);
        static_cast<QSGFlatColorMaterial*>(bgNode->material())->setColor(m_frame.backgroundColor);
        bgNode->markDirty(QSGNode::DirtyGeometry | QSGNode::DirtyMaterial);
    }

    // Selection background overlay (only visible when a selection exists)
    {
        fillSelectionGeometry(selBgNode->geometry(), w);
        static_cast<QSGFlatColorMaterial*>(selBgNode->material())->setColor(m_frame.selectedBackgroundColor);
        selBgNode->markDirty(QSGNode::DirtyGeometry | QSGNode::DirtyMaterial);
    }

    // Two extra degenerate vertices break the strip between stereo channels so
    // no triangles bridge the end of one channel to the start of the next.
    const bool hasSplit = m_frame.channelSplitIndex > 0 && m_frame.channelSplitIndex < m_frame.vertices.size();
    const int splitIndex = static_cast<int>(m_frame.channelSplitIndex);

    // Waveform body — triangle strip: (x, maxY), (x, minY) pairs per column
    {
        auto* geo = waveformNode->geometry();
        geo->allocate(N * 2 + (hasSplit ? 2 : 0));
        auto* v = geo->vertexDataAsColoredPoint2D();
        int vi = 0;
        for (int i = 0; i < N; ++i) {
            const auto& col = m_frame.vertices[i];
            if (hasSplit && i == splitIndex) {
                const auto& prev = m_frame.vertices[i - 1];
                v[vi++].set(prev.x, prev.minY, prev.r, prev.g, prev.b, prev.a);
                v[vi++].set(col.x, col.maxY, col.r, col.g, col.b, col.a);
            }
            v[vi++].set(col.x, col.maxY, col.r, col.g, col.b, col.a);
            v[vi++].set(col.x, col.minY, col.r, col.g, col.b, col.a);
        }
        waveformNode->markDirty(QSGNode::DirtyGeometry);
    }

    // RMS band — only allocate vertices when enabled
    {
        auto* geo = rmsNode->geometry();
        if (m_frame.showRMS) {
            geo->allocate(N * 2 + (hasSplit ? 2 : 0));
            auto* v = geo->vertexDataAsColoredPoint2D();
            int vi = 0;
            for (int i = 0; i < N; ++i) {
                const auto& col = m_frame.vertices[i];
                if (hasSplit && i == splitIndex) {
                    const auto& prev = m_frame.vertices[i - 1];
                    v[vi++].set(prev.x, prev.rmsMinY, prev.rmsR, prev.rmsG, prev.rmsB, prev.rmsA);
                    v[vi++].set(col.x, col.rmsMaxY, col.rmsR, col.rmsG, col.rmsB, col.rmsA);
                }
                v[vi++].set(col.x, col.rmsMaxY, col.rmsR, col.rmsG, col.rmsB, col.rmsA);
                v[vi++].set(col.x, col.rmsMinY, col.rmsR, col.rmsG, col.rmsB, col.rmsA);
            }
        } else {
            geo->allocate(0);
        }
        rmsNode->markDirty(QSGNode::DirtyGeometry);
    }

    fillTriangles(samplesNode, m_frame.sampleTriangles);
    fillTriangles(sampleHeadsNode, m_frame.sampleHeadTriangles);

    // Zero line — one 1px line per channel
    {
        auto* geo = zeroLineNode->geometry();
        geo->setDrawingMode(QSGGeometry::DrawTriangles);
        geo->allocate(static_cast<int>(m_frame.zeroLineYs.size()) * 6);
        auto* v = geo->vertexDataAsPoint2D();
        int vi = 0;
        for (float y : m_frame.zeroLineYs) {
            v[vi++].set(0, y - 0.5f);
            v[vi++].set(w, y - 0.5f);
            v[vi++].set(0, y + 0.5f);
            v[vi++].set(w, y - 0.5f);
            v[vi++].set(w, y + 0.5f);
            v[vi++].set(0, y + 0.5f);
        }
        static_cast<QSGFlatColorMaterial*>(zeroLineNode->material())->setColor(m_frame.zeroLineColor);
        zeroLineNode->markDirty(QSGNode::DirtyGeometry | QSGNode::DirtyMaterial);
    }

    updateClipRect(static_cast<QSGClipNode*>(root), w, m_frame.height);

    m_dirty = false;
    m_nodeValid = true;
    m_appliedContentOffset = m_contentOffset;
    m_appliedWidth = w;

    return root;
}
