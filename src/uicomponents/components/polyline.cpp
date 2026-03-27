/*
 * Audacity: A Digital Audio Editor
 */
#include "polyline.h"
#include "realfn.h"

#include <QPainter>
#include <QPen>
#include <QBrush>
#include <QHoverEvent>
#include <QMouseEvent>

#include <algorithm>
#include <cmath>

namespace {
constexpr double MOVE_THRESHOLD = 3.0;
constexpr double EPSILON = 1e-12;
constexpr double BOUNDARY_MARGIN = 0.1;

static constexpr int INVALID_POINT_IDX = -1;
static constexpr int PENDING_POINT_IDX = -2;

static inline qreal toPxX(const QQuickItem* item, qreal xN)
{
    return xN * item->width();
}

static inline qreal toPxY(const QQuickItem* item, qreal yN)
{
    const qreal h = item->height();
    if (h <= 0.0) {
        return 0.0;
    }

    // NOTE: preserve full height of the line at top/bottom edges
    const auto* polyline = static_cast<const Polyline*>(item);
    const qreal inset = std::clamp(polyline->lineWidth() * 0.5, 0.0, h * 0.5);
    const qreal drawableHeight = h - (2.0 * inset);

    return inset + (1.0 - yN) * drawableHeight;
}

static qreal pointToSegmentDistance(const QPointF& point, const QPointF& segmentStart, const QPointF& segmentEnd)
{
    const QPointF segment = segmentEnd - segmentStart;
    const qreal segmentLengthSquared = QPointF::dotProduct(segment, segment);
    if (segmentLengthSquared <= EPSILON) {
        return std::hypot(point.x() - segmentStart.x(), point.y() - segmentStart.y());
    }

    const QPointF segmentStartToPoint = point - segmentStart;
    qreal t = QPointF::dotProduct(segmentStartToPoint, segment) / segmentLengthSquared;
    t = std::max<qreal>(0.0, std::min<qreal>(1.0, t));

    const QPointF closestPoint = segmentStart + segment * t;
    return std::hypot(point.x() - closestPoint.x(), point.y() - closestPoint.y());
}

static GhostPoint ghostPointToSegmentDistance(const QPointF& point, const QPointF& segmentStart, const QPointF& segmentEnd)
{
    const QPointF segment = segmentEnd - segmentStart;
    const qreal segmentLengthSquared = QPointF::dotProduct(segment, segment);
    if (segmentLengthSquared <= EPSILON) {
        const qreal distance = std::hypot(point.x() - segmentStart.x(), point.y() - segmentStart.y());
        return { segmentStart, distance };
    }

    const QPointF segmentStartToPoint = point - segmentStart;
    qreal t = QPointF::dotProduct(segmentStartToPoint, segment) / segmentLengthSquared;
    t = std::max<qreal>(0.0, std::min<qreal>(1.0, t));

    const QPointF closestPoint = segmentStart + segment * t;
    const qreal distance = std::hypot(point.x() - closestPoint.x(), point.y() - closestPoint.y());
    return { closestPoint, distance };
}

// NOTE: can be replaced with std::lerp in C++20
static double lerp(double a, double b, double t)
{
    return a + (b - a) * t;
}

// compute Y at X using linear interpolation
static double valueAtX(const QVector<QPointF>& sortedPoints, double x)
{
    if (sortedPoints.isEmpty()) {
        return 0.0;
    }

    if (x <= sortedPoints.front().x()) {
        return sortedPoints.front().y();
    }

    if (x >= sortedPoints.back().x()) {
        return sortedPoints.back().y();
    }

    for (int i = 0; i < sortedPoints.size() - 1; ++i) {
        const auto& a = sortedPoints[i];
        const auto& b = sortedPoints[i + 1];
        if (x >= a.x() && x <= b.x()) {
            const double dx = b.x() - a.x();
            if (std::abs(dx) <= EPSILON) {
                return a.y();
            }
            const double t = (x - a.x()) / dx;
            return lerp(a.y(), b.y(), t);
        }
    }

    return sortedPoints.back().y();
}
}

Polyline::Polyline(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setAcceptHoverEvents(true);
    setAcceptedMouseButtons(Qt::LeftButton);

    setAntialiasing(true);
    setRenderTarget(QQuickPaintedItem::FramebufferObject);
    setOpaquePainting(false);
}

void Polyline::init()
{
    dispatcher()->reg(this, "action://cancel", [this](){
        // emit signal and let decide model what to do
        emit dragCancelled();
        resetGestureState();
    });
}

QColor Polyline::lineColor() const
{
    return m_lineColor;
}

void Polyline::setLineColor(const QColor& c)
{
    if (m_lineColor == c) {
        return;
    }

    m_lineColor = c;
    emit lineColorChanged();
    update();
}

qreal Polyline::lineWidth() const
{
    return m_lineWidth;
}

void Polyline::setLineWidth(qreal w)
{
    if (m_lineWidth == w) {
        return;
    }

    m_lineWidth = w;
    emit lineWidthChanged();

    update();
}

qreal Polyline::baselineN() const
{
    return m_baselineN;
}

void Polyline::setBaselineN(qreal v)
{
    v = clamp01(v);

    if (m_baselineN == v) {
        return;
    }

    m_baselineN = v;
    emit baselineNChanged();

    update();
}

qreal Polyline::pointRadius() const
{
    return m_pointRadius;
}

void Polyline::setPointRadius(qreal r)
{
    if (m_pointRadius == r) {
        return;
    }

    m_pointRadius = r;
    emit pointRadiusChanged();

    update();
}

qreal Polyline::ghostPointRadius() const
{
    return m_ghostPointRadius;
}

void Polyline::setGhostPointRadius(qreal r)
{
    if (m_ghostPointRadius == r) {
        return;
    }

    m_ghostPointRadius = r;
    emit ghostPointRadiusChanged();

    update();
}

qreal Polyline::pointOutlineWidth() const
{
    return m_pointOutlineWidth;
}

void Polyline::setPointOutlineWidth(qreal w)
{
    if (m_pointOutlineWidth == w) {
        return;
    }

    m_pointOutlineWidth = w;
    emit pointOutlineWidthChanged();
}

QColor Polyline::pointOutlineColor() const
{
    return m_pointOutlineColor;
}

void Polyline::setPointOutlineColor(const QColor& c)
{
    if (m_pointOutlineColor == c) {
        return;
    }

    m_pointOutlineColor = c;
    emit pointOutlineColorChanged();
}

QColor Polyline::pointCentreColor() const
{
    return m_pointCentreColor;
}

void Polyline::setPointCentreColor(const QColor& c)
{
    if (m_pointCentreColor == c) {
        return;
    }

    m_pointCentreColor = c;
    emit pointCentreColorChanged();
}

QColor Polyline::ghostPointOutlineColor() const
{
    return m_ghostPointOutlineColor;
}

void Polyline::setGhostPointOutlineColor(const QColor& c)
{
    if (m_ghostPointOutlineColor == c) {
        return;
    }

    m_ghostPointOutlineColor = c;
    emit ghostPointOutlineColorChanged();
}

qreal Polyline::hitRadius() const
{
    return m_hitRadius;
}

void Polyline::setHitRadius(qreal r)
{
    if (m_hitRadius == r) {
        return;
    }

    m_hitRadius = r;
    emit hitRadiusChanged();
}

QVector<QPointF> Polyline::points() const
{
    return m_points;
}

void Polyline::setPoints(const QVector<QPointF>& pts)
{
    if (m_points == pts) {
        if (m_points.isEmpty()) {
            updateBaselineFromDefaultValue();
            update();
        }
        return;
    }
    m_points = pts;
    emit pointsChanged();

    if (m_points.isEmpty()) {
        updateBaselineFromDefaultValue();
    }

    // if there's pending point, find its index and mark as ready for drag
    if (m_pressed && m_pressedPointIndex == PENDING_POINT_IDX && m_pressedOnLine) {
        if (width() > 0 && height() > 0 && !m_points.isEmpty()) {
            const auto ghostPoint = ghostPointToPolylinePx(m_pressPx);

            QPointF pN(clamp01(ghostPoint.point.x() / width()),
                       1.0 - clamp01(ghostPoint.point.y() / height()));

            const QPointF targetDomain = domainFromNormalized(pN);

            int bestIdx = INVALID_POINT_IDX;
            double bestScore = std::numeric_limits<double>::max();

            for (int i = 0; i < m_points.size(); ++i) {
                const double dx = std::abs(m_points[i].x() - targetDomain.x());
                const double dy = std::abs(m_points[i].y() - targetDomain.y());
                const double score = dx * 1000.0 + dy;
                if (score < bestScore) {
                    bestScore = score;
                    bestIdx = i;
                }
            }

            if (bestIdx >= 0) {
                m_pressedOnPoint = true;
                m_pressedPointIndex = bestIdx;
                m_draggedPointDomain = m_points[bestIdx];
                m_hasDraggedPointDomain = true;
            }
        }
    }

    rebuildVisiblePoints();
}

qreal Polyline::defaultValue() const
{
    return m_defaultValue;
}

void Polyline::setXRangeFrom(qreal v)
{
    if (m_xFrom == v) {
        return;
    }

    m_xFrom = v;
    emit xRangeFromChanged();

    rebuildVisiblePoints();
}

qreal Polyline::xRangeTo() const
{
    return m_xTo;
}

void Polyline::setXRangeTo(qreal v)
{
    if (m_xTo == v) {
        return;
    }

    m_xTo = v;
    emit xRangeToChanged();

    rebuildVisiblePoints();
}

qreal Polyline::yRangeFrom() const
{
    return m_yFrom;
}

void Polyline::setYRangeFrom(qreal v)
{
    if (m_yFrom == v) {
        return;
    }

    m_yFrom = v;
    emit yRangeFromChanged();

    rebuildVisiblePoints();
}

qreal Polyline::yRangeTo() const
{
    return m_yTo;
}

void Polyline::setYRangeTo(qreal v)
{
    if (m_yTo == v) {
        return;
    }

    m_yTo = v;
    emit yRangeToChanged();

    rebuildVisiblePoints();
}

qreal Polyline::ySplitNormalized() const
{
    return m_ySplitNormalized;
}

void Polyline::setYSplitNormalized(qreal v)
{
    v = clamp01(v);
    if (m_ySplitNormalized == v) {
        return;
    }

    m_ySplitNormalized = v;
    emit ySplitNormalizedChanged();

    rebuildVisiblePoints();
}

qreal Polyline::ySplitValue() const
{
    return m_ySplitValue;
}

void Polyline::setYSplitValue(qreal v)
{
    const qreal low = std::min(m_yFrom, m_yTo);
    const qreal high = std::max(m_yFrom, m_yTo);
    qreal clampedValue = std::clamp(v, low, high);

    if (m_ySplitValue == clampedValue) {
        return;
    }

    m_ySplitValue = clampedValue;
    emit ySplitValueChanged();

    rebuildVisiblePoints();
}

bool Polyline::yAxisInverse() const
{
    return m_yAxisInverse;
}

void Polyline::setYAxisInverse(bool v)
{
    if (m_yAxisInverse == v) {
        return;
    }

    m_yAxisInverse = v;
    emit yAxisInverseChanged();
    rebuildVisiblePoints();
}

bool Polyline::hasActivePoint() const
{
    return m_hasActivePoint;
}

qreal Polyline::activePointX() const
{
    return m_activePointPx.x();
}

qreal Polyline::activePointY() const
{
    return m_activePointPx.y();
}

qreal Polyline::activePointValue() const
{
    return m_activePointValue;
}

void Polyline::setDefaultValue(qreal v)
{
    if (m_defaultValue == v) {
        return;
    }

    m_defaultValue = v;
    emit defaultValueChanged();

    // if there are no points, baseline should reflect defaultY immediately
    if (m_points.isEmpty()) {
        updateBaselineFromDefaultValue();
        rebuildVisiblePoints();
    }
}

qreal Polyline::xRangeFrom() const
{
    return m_xFrom;
}

qreal Polyline::clamp01(qreal v) const
{
    return std::max<qreal>(0.0, std::min<qreal>(1.0, v));
}

QPointF Polyline::clamp01(const QPointF& p) const
{
    return QPointF(clamp01(p.x()), clamp01(p.y()));
}

bool Polyline::hasValidXRange() const
{
    return std::isfinite(m_xFrom) && std::isfinite(m_xTo) && std::abs(m_xTo - m_xFrom) > EPSILON;
}

bool Polyline::hasValidYRange() const
{
    return std::isfinite(m_yFrom) && std::isfinite(m_yTo) && std::abs(m_yTo - m_yFrom) > EPSILON;
}

bool Polyline::hasValidYSplit() const
{
    return hasValidYRange() && !muse::RealIsEqualOrLess(m_ySplitNormalized, 0.0) && !muse::RealIsEqualOrMore(m_ySplitNormalized, 1.0);
}

qreal Polyline::yDomainFromNormalized(qreal yNormalized) const
{
    const qreal n = clamp01(yNormalized);

    // simple linear mapping (no split)
    if (!hasValidYSplit()) {
        return m_yFrom + n * (m_yTo - m_yFrom);
    }

    const qreal splitNormalized = m_ySplitNormalized;
    const qreal splitValue      = m_ySplitValue;

    // first segment (bottom → split)
    if (n <= splitNormalized) {
        if (splitNormalized <= EPSILON) {
            return splitValue;
        }

        const qreal segmentN = n / splitNormalized;
        return m_yFrom + segmentN * (splitValue - m_yFrom);
    }

    // second segment (split → top)
    const qreal secondSegmentSize = 1.0 - splitNormalized;
    if (secondSegmentSize <= EPSILON) {
        return splitValue;
    }

    const qreal segmentN = (n - splitNormalized) / secondSegmentSize;
    return splitValue + segmentN * (m_yTo - splitValue);
}

qreal Polyline::yNormalizedFromDomain(qreal yDomain) const
{
    // simple linear mapping (no split)
    if (!hasValidYSplit()) {
        return clamp01((yDomain - m_yFrom) / (m_yTo - m_yFrom));
    }

    const qreal splitNormalized = m_ySplitNormalized;
    const qreal splitValue      = m_ySplitValue;

    const qreal minY = std::min(m_yFrom, m_yTo);
    const qreal maxY = std::max(m_yFrom, m_yTo);
    const qreal yClamped = std::clamp(yDomain, minY, maxY);

    const bool increasing = (m_yTo >= m_yFrom);

    const bool isInFirstSegment
        =increasing ? (yClamped <= splitValue)
          : (yClamped >= splitValue);

    // ---- first segment ----
    if (isInFirstSegment) {
        const qreal segmentSize = splitValue - m_yFrom;
        if (std::abs(segmentSize) <= EPSILON) {
            return splitNormalized;
        }

        const qreal segmentN = (yClamped - m_yFrom) / segmentSize;
        return clamp01(segmentN * splitNormalized);
    }

    // ---- second segment ----
    const qreal segmentSize = m_yTo - splitValue;
    if (std::abs(segmentSize) <= EPSILON) {
        return splitNormalized;
    }

    const qreal segmentN = (yClamped - splitValue) / segmentSize;
    return clamp01(splitNormalized + segmentN * (1.0 - splitNormalized));
}

void Polyline::updateBaselineFromDefaultValue()
{
    const qreal baseline = normalizedFromDomain(QPointF(m_xFrom, m_defaultValue)).y();
    if (m_baselineN == baseline) {
        return;
    }

    m_baselineN = baseline;
    emit baselineNChanged();
}

void Polyline::updateActivePoint()
{
    const int hoveredDomainIdx = pointIndexAtPx(m_hoverPx);
    int draggedDisplayDomainIdx = INVALID_POINT_IDX;
    if (m_pressed && m_hasDraggedPointDomain && !m_points.isEmpty()) {
        double bestScore = std::numeric_limits<double>::max();
        for (int i = 0; i < m_points.size(); ++i) {
            const double dx = std::abs(m_points[i].x() - m_draggedPointDomain.x());
            const double dy = std::abs(m_points[i].y() - m_draggedPointDomain.y());
            const double score = dx * 1000.0 + dy;
            if (score < bestScore) {
                bestScore = score;
                draggedDisplayDomainIdx = i;
            }
        }
    }

    const int activeDomainIdx = (draggedDisplayDomainIdx >= 0)
                                ? draggedDisplayDomainIdx
                                : ((m_pressedPointIndex >= 0) ? m_pressedPointIndex : hoveredDomainIdx);

    if (activeDomainIdx < 0) {
        if (m_hasActivePoint) {
            m_hasActivePoint = false;
            m_activePointPx = {};
            m_activePointValue = 0.0;
            emit activePointChanged();
        }
        return;
    }

    QPointF activeN;
    bool found = false;
    for (int i = 0; i < m_pointsNVisible.size() && i < m_visibleToDomainIndex.size(); ++i) {
        if (m_visibleToDomainIndex[i] == activeDomainIdx) {
            activeN = m_pointsNVisible[i];
            found = true;
            break;
        }
    }

    if (!found) {
        if (m_hasActivePoint) {
            m_hasActivePoint = false;
            m_activePointPx = {};
            m_activePointValue = 0.0;
            emit activePointChanged();
        }
        return;
    }

    const QPointF newPx(toPxX(this, activeN.x()), toPxY(this, activeN.y()));
    const qreal newValue = (draggedDisplayDomainIdx >= 0 && m_hasDraggedPointDomain)
                           ? m_draggedPointDomain.y()
                           : m_points[activeDomainIdx].y();
    const bool changed = (!m_hasActivePoint
                          || newPx != m_activePointPx
                          || std::abs(newValue - m_activePointValue) > EPSILON);

    m_hasActivePoint = true;
    m_activePointPx = newPx;
    m_activePointValue = newValue;

    if (changed) {
        emit activePointChanged();
    }
}

QPointF Polyline::normalizedFromDomain(const QPointF& p) const
{
    if (!hasValidXRange() || !hasValidYRange()) {
        return clamp01(QPointF(0.0, 0.0));
    }

    const qreal xN = (p.x() - m_xFrom) / (m_xTo - m_xFrom);
    qreal yN = yNormalizedFromDomain(p.y());

    if (m_yAxisInverse) {
        yN = 1.0 - yN;
    }

    return clamp01(QPointF(xN, yN));
}

QPointF Polyline::domainFromNormalized(const QPointF& pN) const
{
    if (!hasValidXRange() || !hasValidYRange()) {
        return QPointF(m_xFrom, m_yFrom);
    }

    const qreal x = m_xFrom + clamp01(pN.x()) * (m_xTo - m_xFrom);

    qreal yT = clamp01(pN.y());
    if (m_yAxisInverse) {
        yT = 1.0 - yT;
    }
    const qreal y = yDomainFromNormalized(yT);

    return QPointF(x, y);
}

QVector<QPointF> Polyline::normalizedFromDomain(const QVector<QPointF>& pts) const
{
    QVector<QPointF> out;
    out.reserve(pts.size());
    for (const auto& p : pts) {
        out.push_back(normalizedFromDomain(p));
    }

    return out;
}

QVector<QPointF> Polyline::domainFromNormalized(const QVector<QPointF>& ptsN) const
{
    QVector<QPointF> out;
    out.reserve(ptsN.size());
    for (const auto& pN : ptsN) {
        out.push_back(domainFromNormalized(pN));
    }

    return out;
}

void Polyline::rebuildVisiblePoints()
{
    m_pointsNVisible.clear();
    m_visibleToDomainIndex.clear();

    const double xRange = (m_xTo - m_xFrom);
    const double yRange = (m_yTo - m_yFrom);

    if (width() <= 0 || height() <= 0 || std::abs(xRange) <= 0 || std::abs(yRange) <= 0) {
        if (m_points.isEmpty()) {
            updateBaselineFromDefaultValue();
        }
        update();
        return;
    }

    if (m_points.isEmpty()) {
        updateBaselineFromDefaultValue();
        update();
        return;
    }

    struct P {
        QPointF p;
        int idx;
    };

    QVector<P> sortedPointsWithIndexes;
    sortedPointsWithIndexes.reserve(m_points.size());
    for (int i = 0; i < m_points.size(); ++i) {
        sortedPointsWithIndexes.push_back({ m_points[i], i });
    }
    std::sort(sortedPointsWithIndexes.begin(), sortedPointsWithIndexes.end(),
              [](const P& a, const P& b) { return a.p.x() < b.p.x(); });

    auto normY = [&](double yAbs) {
        double yn = yNormalizedFromDomain(yAbs);
        if (m_yAxisInverse) {
            yn = 1.0 - yn;
        }
        return std::clamp(yn, 0.0, 1.0);
    };

    // interpolate at window edges
    QVector<QPointF> sortedPointsN;
    sortedPointsN.reserve(sortedPointsWithIndexes.size());
    for (const auto& it : sortedPointsWithIndexes) {
        sortedPointsN.push_back(QPointF(it.p.x(), normY(it.p.y())));
    }

    // Synthetic boundary points must be interpolated in rendered Y-space.
    // Interpolating in domain Y breaks continuity for split mappings
    // (e.g. segments crossing 0 dB split value).
    const double yAt0N = valueAtX(sortedPointsN, m_xFrom);
    const double yAt1N = valueAtX(sortedPointsN, m_xTo);

    // NOTE: synthetic boundary points are added just outside the visible range
    // so the polyline draws correctly: they allow horizontal segments
    // before the first real point and after the last real point. Especially in
    // cases where polyline is currently visible only partially the screen.
    // These are rendering-only points (no corresponding domain index).

    // left boundary (synthetic)
    m_pointsNVisible.push_back(QPointF(-0.1, std::clamp(yAt0N, 0.0, 1.0)));
    m_visibleToDomainIndex.push_back(INVALID_POINT_IDX);

    // interior real points
    for (const auto& it : sortedPointsWithIndexes) {
        const auto& p = it.p;
        // NOTE: build visible points with a margin so points directly at the edges
        // of container do not flash on re-paint
        if (p.x() < (m_xFrom - BOUNDARY_MARGIN) || p.x() > (m_xTo + BOUNDARY_MARGIN)) {
            continue;
        }
        const double xN = (p.x() - m_xFrom) / xRange;
        m_pointsNVisible.push_back(QPointF(std::clamp(xN, 0.0, 1.0), normY(p.y())));
        m_visibleToDomainIndex.push_back(it.idx);
    }

    // right boundary (synthetic)
    m_pointsNVisible.push_back(QPointF(1.1, std::clamp(yAt1N, 0.0, 1.0)));
    m_visibleToDomainIndex.push_back(INVALID_POINT_IDX);

    updateActivePoint();
    update();
}

QVector<QPointF> Polyline::polylinePx() const
{
    QVector<QPointF> pts;

    if (width() <= 0 || height() <= 0) {
        return pts;
    }

    // 0 or 1 point -> horizontal baseline
    if (m_pointsNVisible.size() < 2) {
        qreal yN = m_baselineN;
        if (m_pointsNVisible.size() == 1) {
            yN = m_pointsNVisible[0].y();
        }
        yN = clamp01(yN);

        const qreal y = toPxY(this, yN);
        pts.push_back(QPointF(0.0, y));
        pts.push_back(QPointF(width(), y));

        return pts;
    }

    // 2+ points -> polyline
    QVector<QPointF> sorted = m_pointsNVisible;
    std::sort(sorted.begin(), sorted.end(),
              [](const QPointF& a, const QPointF& b) { return a.x() < b.x(); });

    const QPointF firstN = sorted.front();
    const QPointF lastN  = sorted.back();

    const qreal firstYpx = toPxY(this, clamp01(firstN.y()));
    const qreal lastYpx  = toPxY(this, clamp01(lastN.y()));

    pts.reserve(sorted.size() + 2);

    // left horizontal segment start
    pts.push_back(QPointF(0.0, firstYpx));

    // actual points
    for (const auto& pN : sorted) {
        pts.push_back(QPointF(toPxX(this, clamp01(pN.x())),
                              toPxY(this, clamp01(pN.y()))));
    }

    // right horizontal segment end
    pts.push_back(QPointF(width(), lastYpx));

    return pts;
}

bool Polyline::isNearLinePx(const QPointF& px) const
{
    const auto pts = polylinePx();
    if (pts.size() < 2) {
        return false;
    }

    qreal best = std::numeric_limits<qreal>::max();
    for (int i = 0; i < pts.size() - 1; ++i) {
        best = std::min(best, pointToSegmentDistance(px, pts[i], pts[i + 1]));
    }
    return best <= m_hitRadius;
}

int Polyline::pointIndexAtPx(const QPointF& px) const
{
    // search in visible points, skip synthetic boundary points
    for (int i = 0; i < m_pointsNVisible.size(); ++i) {
        const int domainIdx = (i < m_visibleToDomainIndex.size()) ? m_visibleToDomainIndex[i] : INVALID_POINT_IDX;
        if (domainIdx < 0) {
            continue;
        }

        QPointF pN = m_pointsNVisible[i];
        const qreal x = toPxX(this, pN.x());
        const qreal y = toPxY(this, pN.y());
        const qreal dx = px.x() - x;
        const qreal dy = px.y() - y;
        if ((dx * dx + dy * dy) <= (m_hitRadius * m_hitRadius)) {
            return domainIdx;
        }
    }

    return INVALID_POINT_IDX;
}

GhostPoint Polyline::ghostPointToPolylinePx(const QPointF& px) const
{
    GhostPoint best;

    const auto pts = polylinePx();
    if (pts.size() < 2) {
        best.point = px;
        best.distToSegment = std::numeric_limits<qreal>::max();
        return best;
    }

    for (int i = 0; i < pts.size() - 1; ++i) {
        const auto res = ghostPointToSegmentDistance(px, pts[i], pts[i + 1]);
        if (res.distToSegment < best.distToSegment) {
            best = res;
        }
    }

    return best;
}

void Polyline::updateCursor()
{
    const bool interactive = m_hoveredOnLine || m_pressed || (m_pressedPointIndex >= 0);

    if (interactive) {
        setCursor(Qt::CrossCursor);
    } else {
        unsetCursor();
    }
}

void Polyline::resetGestureState()
{
    m_pressed = false;
    m_pressedOnLine = false;
    m_pressedOnPoint = false;
    m_pressedPointIndex = INVALID_POINT_IDX;
    m_hasDraggedPointDomain = false;
    m_draggedPointDomain = {};
    m_movedSincePress = false;
    m_pressPx = QPointF(0.0, 0.0);

    updateCursor();
    updateActivePoint();
    update();
}

void Polyline::geometryChange(const QRectF& newG, const QRectF& oldG)
{
    QQuickPaintedItem::geometryChange(newG, oldG);
    if (newG.size() != oldG.size()) {
        rebuildVisiblePoints();
    }
}

void Polyline::paint(QPainter* painter)
{
    if (!painter) {
        return;
    }

    painter->setRenderHint(QPainter::Antialiasing, antialiasing());

    // draw background overlay
    {
        QColor overlay = uiConfiguration()->currentTheme().extra["black_color"].value<QColor>();
        overlay.setAlphaF(0.25);

        painter->setPen(Qt::NoPen);
        painter->setBrush(overlay);
        painter->drawRect(boundingRect());
    }

    // draw polyline
    {
        QPen pen(m_lineColor);
        pen.setWidthF(m_lineWidth);
        pen.setJoinStyle(Qt::MiterJoin);
        painter->setPen(pen);
        painter->setBrush(Qt::NoBrush);

        const auto pts = polylinePx();
        if (pts.size() >= 2) {
            for (int i = 0; i < pts.size() - 1; ++i) {
                painter->drawLine(pts[i], pts[i + 1]);
            }
        }
    }

    // draw points
    painter->setPen(Qt::NoPen);
    painter->setBrush(m_lineColor);

    const int n = m_pointsNVisible.size();
    const int hoveredIndex = pointIndexAtPx(m_hoverPx);
    for (int i = 0; i < n; ++i) {
        QPointF pN = m_pointsNVisible[i];
        const QPointF c(toPxX(this, pN.x()), toPxY(this, pN.y()));

        const int domainIdx
            =(i < m_visibleToDomainIndex.size()) ? m_visibleToDomainIndex[i] : INVALID_POINT_IDX;
        const bool isHovered = (domainIdx >= 0 && domainIdx == hoveredIndex);
        if (isHovered) {
            const qreal regularOuterRadius = m_pointRadius + (m_pointOutlineWidth * 0.5);
            const qreal hoveredOuterRadius = regularOuterRadius + 1.0;

            painter->setPen(Qt::NoPen);
            painter->setBrush(m_pointOutlineColor);
            painter->drawEllipse(c, hoveredOuterRadius, hoveredOuterRadius);

            painter->setBrush(Qt::black);
            painter->drawEllipse(c, m_pointRadius - 1.0, m_pointRadius - 1.0);

            painter->setBrush(Qt::white);
            painter->drawEllipse(c, m_pointRadius - 3.0, m_pointRadius - 3.0);
        } else {
            // fill
            painter->setPen(Qt::NoPen);
            painter->setBrush(m_pointCentreColor);
            painter->drawEllipse(c, m_pointRadius, m_pointRadius);

            // outline
            QPen innerPen(m_pointOutlineColor);
            innerPen.setWidthF(m_pointOutlineWidth);
            painter->setPen(innerPen);
            painter->setBrush(Qt::NoBrush);

            painter->drawEllipse(c, m_pointRadius, m_pointRadius);
        }
    }

    // draw hover ghost point
    if (m_hoveredOnLine && m_pressedPointIndex < 0) {
        QPointF hp = m_hoverGhostPx;

        if (m_pointsNVisible.size() < 2) {
            const qreal yN
                =(m_pointsNVisible.size() == 1)
                  ? m_pointsNVisible[0].y()
                  : m_baselineN;

            hp.setY(toPxY(this, yN));
        }

        if (pointIndexAtPx(hp) < 0 && isNearLinePx(hp)) {
            // fill
            painter->setPen(Qt::NoPen);
            painter->setBrush(m_ghostPointOutlineColor);
            painter->drawEllipse(hp, m_ghostPointRadius, m_ghostPointRadius);

            // outline
            QPen innerPen(m_ghostPointOutlineColor);
            innerPen.setWidthF(m_pointOutlineWidth);
            painter->setPen(innerPen);
            painter->setBrush(Qt::NoBrush);

            painter->drawEllipse(hp, m_ghostPointRadius, m_ghostPointRadius);
        }
    }
}

void Polyline::hoverMoveEvent(QHoverEvent* e)
{
    // NOTE: even if mouse is still, Qt produces hoverMoveEvents constantly
    if (m_hoverPx == e->position()) {
        // IMPORTANT: let through hover events (otherwise double-click
        // clip selection may be broken)
        e->ignore();
        return;
    }

    m_hoverPx = e->position();

    const bool nearPoint = (pointIndexAtPx(m_hoverPx) >= 0);

    auto proj = ghostPointToPolylinePx(m_hoverPx);
    const bool nearLine = (proj.distToSegment <= m_hitRadius);

    m_hoveredOnLine = (nearPoint || nearLine);
    updateCursor();

    if (m_pointsNVisible.size() >= 2) {
        m_hoverGhostPx = proj.point;
    } else {
        m_hoverGhostPx = m_hoverPx;
    }

    updateActivePoint();
    update();

    // IMPORTANT: only consume hover when automation is actually interactive.
    // Otherwise let underlying ClipItem hover area keep containsMouse=true,
    // so itemHovered stays correct for selection/double-click logic.
    if (nearPoint || nearLine || m_pressed || m_pressedPointIndex >= 0) {
        e->accept();
    } else {
        e->ignore();
    }
}

void Polyline::hoverLeaveEvent(QHoverEvent* e)
{
    Q_UNUSED(e);
    m_hoveredOnLine = false;
    updateCursor();
    updateActivePoint();
    update();
}

void Polyline::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) {
        e->ignore();
        return;
    }

    const int pointIndex = pointIndexAtPx(e->position());
    const bool onPoint = pointIndex >= 0;
    const bool onLine  = isNearLinePx(e->position());

    // NOTE: allow clicks on the points and lines only
    if (!onPoint && !onLine) {
        e->ignore();
        return;
    }

    resetGestureState();

    e->accept();
    updateCursor();

    m_pressed = true;
    m_pressPx = e->position();

    if (onPoint) {
        m_pressedOnPoint = true;
        m_pressedPointIndex = pointIndex;
        if (m_pressedPointIndex >= 0 && m_pressedPointIndex < m_points.size()) {
            m_draggedPointDomain = m_points[m_pressedPointIndex];
            m_hasDraggedPointDomain = true;
        }
        updateActivePoint();
        return;
    }

    if (onLine) {
        m_pressedOnLine = true;
        m_pressedPointIndex = PENDING_POINT_IDX;
        m_pressedOnPoint = false;

        if (width() > 0 && height() > 0) {
            const auto ghostPoint = ghostPointToPolylinePx(e->position());

            QPointF pN(clamp01(ghostPoint.point.x() / width()),
                       1.0 - clamp01(ghostPoint.point.y() / height()));

            const QPointF pDomain = domainFromNormalized(pN);
            emit pointAdded(pDomain.x(), pDomain.y(), /*completed*/ true);
        }

        updateActivePoint();
        return;
    }
}

void Polyline::mouseMoveEvent(QMouseEvent* e)
{
    if (!m_pressed) {
        e->ignore();
        return;
    }

    e->accept();

    const QPointF pos = e->position();
    if (!m_movedSincePress && (pos - m_pressPx).manhattanLength() > MOVE_THRESHOLD) {
        m_movedSincePress = true;
    }

    // drag point (2+ points only)
    if (m_pressedPointIndex >= 0) {
        if (width() <= 0 || height() <= 0) {
            return;
        }

        QPointF pN(pos.x() / width(), 1.0 - (pos.y() / height()));
        pN = clamp01(pN);

        const QPointF pDomain = domainFromNormalized(pN);
        m_draggedPointDomain = pDomain;
        m_hasDraggedPointDomain = true;
        emit pointMoved(m_pressedPointIndex, pDomain.x(), pDomain.y(), /*completed*/ false);
        updateActivePoint();

        return;
    }
}

void Polyline::mouseReleaseEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton || !m_pressed) {
        e->ignore();
        return;
    }
    e->accept();

    const bool isClick = !m_movedSincePress;
    const QPointF rel = e->position();

    // commit point drag
    if (!isClick && m_pressedPointIndex >= 0) {
        QPointF pN(rel.x() / width(), 1.0 - (rel.y() / height()));
        pN = clamp01(pN);

        const QPointF pDomain = domainFromNormalized(pN);
        m_draggedPointDomain = pDomain;
        m_hasDraggedPointDomain = true;
        emit pointMoved(m_pressedPointIndex, pDomain.x(), pDomain.y(), /*completed*/ true);
        emit interactionFinished();
        resetGestureState();
        return;
    }

    emit interactionFinished();
    resetGestureState();
}

void Polyline::mouseDoubleClickEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) {
        e->ignore();
        return;
    }

    const int idx = pointIndexAtPx(e->position());
    if (idx < 0) {
        e->ignore();
        return;
    }

    e->accept();

    emit pointRemoved(idx, /*completed*/ true);
    emit interactionFinished();

    resetGestureState();
}
