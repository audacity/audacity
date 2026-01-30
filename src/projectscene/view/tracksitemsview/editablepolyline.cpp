#include "editablepolyline.h"

#include <QPainter>
#include <QPen>
#include <QBrush>
#include <QHoverEvent>
#include <QMouseEvent>

#include <algorithm>
#include <cmath>

namespace {
static inline qreal toPxX(const QQuickItem* item, qreal xN)
{
    return xN * item->width();
}

static inline qreal toPxY(const QQuickItem* item, qreal yN)
{
    return yN * item->height();
}

static qreal distPointToSegment(const QPointF& p, const QPointF& a, const QPointF& b)
{
    const QPointF ab = b - a;
    const QPointF ap = p - a;

    const qreal ab2 = QPointF::dotProduct(ab, ab);
    if (ab2 <= 1e-9) {
        return std::hypot(p.x() - a.x(), p.y() - a.y());
    }

    qreal t = QPointF::dotProduct(ap, ab) / ab2;
    t = std::max<qreal>(0.0, std::min<qreal>(1.0, t));
    const QPointF c = a + ab * t;

    return std::hypot(p.x() - c.x(), p.y() - c.y());
}

static GhostPoint ghostPointToSegment(const QPointF& p, const QPointF& a, const QPointF& b)
{
    const QPointF ab = b - a;
    const qreal ab2 = QPointF::dotProduct(ab, ab);
    if (ab2 <= 1e-9) {
        const qreal d = std::hypot(p.x() - a.x(), p.y() - a.y());
        return { a, d };
    }

    const QPointF ap = p - a;
    qreal t = QPointF::dotProduct(ap, ab) / ab2;
    t = std::max<qreal>(0.0, std::min<qreal>(1.0, t));

    const QPointF c = a + ab * t;
    const qreal d = std::hypot(p.x() - c.x(), p.y() - c.y());
    return { c, d };
}
}

EditablePolyline::EditablePolyline(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAcceptHoverEvents(true);
    setAcceptedMouseButtons(Qt::LeftButton);

    setAntialiasing(true);
    setRenderTarget(QQuickPaintedItem::FramebufferObject);
    setOpaquePainting(false);
}

void EditablePolyline::setLineColor(const QColor& c)
{
    if (m_lineColor == c) {
        return;
    }
    m_lineColor = c;
    emit lineColorChanged();
    update();
}

void EditablePolyline::setLineWidth(qreal w)
{
    w = std::max<qreal>(0.5, w);

    if (m_lineWidth == w) {
        return;
    }

    m_lineWidth = w;
    emit lineWidthChanged();

    update();
}

void EditablePolyline::setBaselineN(qreal v)
{
    v = clamp01(v);

    if (m_baselineN == v) {
        return;
    }

    m_baselineN = v;

    emit baselineNChanged();
    update();
}

void EditablePolyline::setPointRadius(qreal r)
{
    r = std::max<qreal>(1.0, r);

    if (m_pointRadius == r) {
        return;
    }

    m_pointRadius = r;
    emit pointRadiusChanged();

    update();
}

void EditablePolyline::setHitRadius(qreal r)
{
    r = std::max<qreal>(2.0, r);

    if (m_hitRadius == r) {
        return;
    }

    m_hitRadius = r;
    emit hitRadiusChanged();
}

void EditablePolyline::setPointsN(const QVector<QPointF>& pts)
{
    if (m_pointsN == pts) {
        return;
    }
    m_pointsN = pts;

    // if first point exists, keep baseline aligned with it
    if (m_pointsN.size() == 1) {
        m_baselineN = clamp01(m_pointsN[0].y());
        emit baselineNChanged();
    }

    emit pointsNChanged();
    update();
}

qreal EditablePolyline::clamp01(qreal v) const
{
    return std::max<qreal>(0.0, std::min<qreal>(1.0, v));
}

QPointF EditablePolyline::clamp01(const QPointF& p) const
{
    return QPointF(clamp01(p.x()), clamp01(p.y()));
}

QVector<QPointF> EditablePolyline::polylinePx() const
{
    QVector<QPointF> pts;

    if (width() <= 0 || height() <= 0) {
        return pts;
    }

    // 0 or 1 point -> horizontal baseline
    if (m_pointsN.size() < 2) {
        qreal yN = m_baselineN;
        if (m_pointsN.size() == 1) {
            yN = m_pointsN[0].y();
        }
        yN = clamp01(yN);

        const qreal y = toPxY(this, yN);
        pts.push_back(QPointF(0.0, y));
        pts.push_back(QPointF(width(), y));
        return pts;
    }

    // 2+ points -> envelope behavior:
    QVector<QPointF> sorted = m_pointsN;
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

bool EditablePolyline::isNearLinePx(const QPointF& px) const
{
    const auto pts = polylinePx();
    if (pts.size() < 2) {
        return false;
    }

    qreal best = 1e18;
    for (int i = 0; i < pts.size() - 1; ++i) {
        best = std::min(best, distPointToSegment(px, pts[i], pts[i + 1]));
    }
    return best <= m_hitRadius;
}

int EditablePolyline::pointIndexAtPx(const QPointF& px) const
{
    for (int i = 0; i < m_pointsN.size(); ++i) {
        const qreal x = toPxX(this, m_pointsN[i].x());
        const qreal y = toPxY(this, m_pointsN[i].y());
        const qreal dx = px.x() - x;
        const qreal dy = px.y() - y;

        if ((dx * dx + dy * dy) <= (m_hitRadius * m_hitRadius)) {
            return i;
        }
    }
    return -1;
}

bool EditablePolyline::isInteractiveAtPx(const QPointF& px) const
{
    return (pointIndexAtPx(px) >= 0) || isNearLinePx(px);
}

int EditablePolyline::insertPointOnLineAtPx(const QPointF& px)
{
    if (width() <= 0 || height() <= 0) {
        return -1;
    }

    const qreal xN = clamp01(px.x() / width());
    const qreal yN = calculateLineYN(xN);

    // prevent duplicates: if near an existing point, return that point instead
    const int hit = pointIndexAtPx(QPointF(toPxX(this, xN), toPxY(this, yN)));
    if (hit >= 0) {
        return hit;
    }

    // insert keeping m_pointsN sorted by x
    QPointF newP(xN, yN);

    int insertAt = m_pointsN.size();
    for (int i = 0; i < m_pointsN.size(); ++i) {
        if (m_pointsN[i].x() > xN) {
            insertAt = i;
            break;
        }
    }

    m_pointsN.insert(insertAt, newP);

    // if this is the first point, baseline follows it
    if (m_pointsN.size() == 1) {
        const qreal newBase = clamp01(m_pointsN[0].y());
        if (!qFuzzyCompare(m_baselineN, newBase)) {
            m_baselineN = newBase;
            emit baselineNChanged();
        }
    }

    emit pointsNChanged();
    update();
    return insertAt;
}

qreal EditablePolyline::calculateLineYN(qreal xN) const
{
    xN = clamp01(xN);

    // 0 or 1 point => horizontal baseline (or single point y)
    if (m_pointsN.size() < 2) {
        return (m_pointsN.size() == 1) ? clamp01(m_pointsN[0].y()) : m_baselineN;
    }

    // 2+ points => polyline, evaluate y at xN by finding line segment and interpolating
    QVector<QPointF> sorted = m_pointsN;
    std::sort(sorted.begin(), sorted.end(),
              [](const QPointF& a, const QPointF& b) { return a.x() < b.x(); });

    // clamp outside range to endpoints
    if (xN <= sorted.front().x()) {
        return clamp01(sorted.front().y());
    }
    if (xN >= sorted.back().x()) {
        return clamp01(sorted.back().y());
    }

    // find line segment
    for (int i = 0; i < sorted.size() - 1; ++i) {
        const QPointF a = sorted[i];
        const QPointF b = sorted[i + 1];
        if (xN >= a.x() && xN <= b.x()) {
            const qreal dx = b.x() - a.x();
            if (std::abs(dx) <= 1e-9) {
                return clamp01(a.y());
            }
            const qreal t = (xN - a.x()) / dx;
            return clamp01(a.y() + t * (b.y() - a.y()));
        }
    }

    // fallback
    return clamp01(sorted.back().y());
}

GhostPoint EditablePolyline::ghostPointToPolylinePx(const QPointF& px) const
{
    GhostPoint best;

    const auto pts = polylinePx();
    if (pts.size() < 2) {
        best.point = px;
        best.dist = 1e18;
        return best;
    }

    for (int i = 0; i < pts.size() - 1; ++i) {
        const auto res = ghostPointToSegment(px, pts[i], pts[i + 1]);
        if (res.dist < best.dist) {
            best = res;
        }
    }

    return best;
}

void EditablePolyline::updateCursor()
{
    const bool interactive = m_hoveredOnLine || m_pressed || m_draggingLine || (m_draggedPointIndex >= 0);

    if (interactive) {
        setCursor(Qt::ArrowCursor);
    } else {
        unsetCursor();
    }
}

void EditablePolyline::resetGestureState()
{
    m_pressed = false;
    m_pressedOnLine = false;
    m_pressedOnPoint = false;
    m_pressedPointIndex = -1;
    m_draggedPointIndex = -1;
    m_draggingLine = false;

    updateCursor();
    update();
}

void EditablePolyline::paint(QPainter* painter)
{
    if (!painter) {
        return;
    }

    painter->setRenderHint(QPainter::Antialiasing, antialiasing());

    // draw line/polyline
    {
        QPen pen(m_lineColor);
        pen.setWidthF(m_lineWidth);
        pen.setCapStyle(Qt::FlatCap);
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

    // draw permanent dots
    painter->setPen(Qt::NoPen);
    painter->setBrush(m_lineColor);
    for (const auto& pN : m_pointsN) {
        const QPointF c(toPxX(this, pN.x()), toPxY(this, pN.y()));
        painter->drawEllipse(c, m_pointRadius, m_pointRadius);
    }

    // hover point
    if (m_hoveredOnLine && !m_draggingLine && m_draggedPointIndex < 0) {
        QPointF hp = m_hoverGhostPx;

        // keep hover point on baseline when 0/1 point
        if (m_pointsN.size() < 2) {
            const qreal yN = (m_pointsN.size() == 1) ? m_pointsN[0].y() : m_baselineN;
            hp.setY(toPxY(this, yN));
        }

        if (pointIndexAtPx(hp) < 0 && isNearLinePx(hp)) {
            const qreal eraseRadius = m_pointRadius + m_lineWidth * 0.75;

            // erase underlying line
            painter->save();
            painter->setCompositionMode(QPainter::CompositionMode_Clear);
            painter->setPen(Qt::NoPen);
            painter->setBrush(Qt::transparent);
            painter->drawEllipse(hp, eraseRadius, eraseRadius);
            painter->restore();

            // draw hollow outline
            QPen hoverPen(m_lineColor);
            hoverPen.setWidthF(1.0);
            hoverPen.setCapStyle(Qt::RoundCap);
            hoverPen.setJoinStyle(Qt::RoundJoin);

            painter->setPen(hoverPen);
            painter->setBrush(Qt::NoBrush);
            painter->drawEllipse(hp, m_pointRadius, m_pointRadius);
        }
    }
}

// ------------------------- Hover events -------------------------

void EditablePolyline::hoverMoveEvent(QHoverEvent* e)
{
    m_hoverPx = e->position();

    // interactive if near point or near line
    const bool nearPoint = (pointIndexAtPx(m_hoverPx) >= 0);

    auto proj = ghostPointToPolylinePx(m_hoverPx);
    const bool nearLine = (proj.dist <= m_hitRadius);

    m_hoveredOnLine = (nearPoint || nearLine);
    updateCursor();

    // for 2+ points, hover circle should follow the polyline
    // for 0/1 point, hover circle can follow the baseline
    if (m_pointsN.size() >= 2) {
        m_hoverGhostPx = proj.point;
    } else {
        m_hoverGhostPx = m_hoverPx;
    }

    update();
    e->accept();
}

void EditablePolyline::hoverLeaveEvent(QHoverEvent* e)
{
    Q_UNUSED(e);
    m_hoveredOnLine = false;
    updateCursor();
    update();
}

void EditablePolyline::mousePressEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) {
        e->ignore();
        return;
    }

    // only handle events over the line/points; otherwise let them pass through
    if (!isInteractiveAtPx(e->position())) {
        e->ignore();
        return;
    }

    e->accept();
    updateCursor();

    m_pressedOnPoint = false;
    m_pressedPointIndex = -1;
    m_pressedOnLine = false;
    m_pressed = true;
    m_movedSincePress = false;
    m_pressPx = e->position();
    m_pressBaselineN = m_baselineN;

    m_draggedPointIndex = -1;
    m_draggingLine = false;

    const int hitPoint = pointIndexAtPx(m_pressPx);
    if (hitPoint >= 0) {
        m_pressedOnPoint = true;
        m_pressedPointIndex = hitPoint;

        if (m_pointsN.size() >= 2) {
            // potential drag — actual drag starts only if mouse moves
            m_draggedPointIndex = hitPoint;
        } else {
            // 0/1 point: pressing the dot should allow dragging the whole line
            m_pressedOnLine = true;
            m_draggedPointIndex = -1;
        }

        e->accept();
        return;
    }

    if (isNearLinePx(m_pressPx)) {
        m_pressedOnLine = true;
        e->accept();
        return;
    }

    e->accept();
}

void EditablePolyline::mouseMoveEvent(QMouseEvent* e)
{
    if (!m_pressed) {
        e->ignore();
        return;
    }
    e->accept();

    const QPointF pos = e->position();
    if (!m_movedSincePress && (pos - m_pressPx).manhattanLength() > 4.0) {
        m_movedSincePress = true;

        // start line dragging only after it becomes a drag gesture
        if (m_pressedOnLine && m_pointsN.size() <= 1) {
            m_draggingLine = true;
            if (m_pointsN.size() == 1) {
                m_pressSinglePointN = m_pointsN[0];
            }
        }
    }

    // drag point (2+ points only)
    if (m_draggedPointIndex >= 0 && m_pointsN.size() >= 2) {
        QPointF pN(pos.x() / width(), pos.y() / height());
        m_pointsN[m_draggedPointIndex] = clamp01(pN);
        emit pointsNChanged();
        update();
        e->accept();
        return;
    }

    // drag entire line (0 or 1 point)
    if (m_draggingLine && m_pointsN.size() <= 1) {
        const qreal dyPx = pos.y() - m_pressPx.y();
        const qreal dyN = dyPx / height();
        const qreal newBaseline = clamp01(m_pressBaselineN + dyN);

        if (!qFuzzyCompare(m_baselineN, newBaseline)) {
            m_baselineN = newBaseline;
            emit baselineNChanged();
        }

        if (m_pointsN.size() == 1) {
            // move the single point with the line
            m_pointsN[0] = QPointF(m_pressSinglePointN.x(), newBaseline);
            emit pointsNChanged();
        }

        update();
        e->accept();
        return;
    }

    e->accept();
}

void EditablePolyline::mouseReleaseEvent(QMouseEvent* e)
{
    if (e->button() != Qt::LeftButton) {
        e->ignore();
        return;
    }

    if (!m_pressed) {
        e->ignore();
        return;
    }

    const QPointF rel = e->position();

    const bool isClick = !m_movedSincePress;

    // point clicked => remove point (only if it wasn't a drag)
    if (isClick && m_pressedOnPoint && m_pressedPointIndex >= 0) {
        if (m_pointsN.size() > 1) {
            m_pointsN.removeAt(m_pressedPointIndex);
            emit pointsNChanged();

            // if we ended up with one point, keep baseline aligned
            if (m_pointsN.size() == 1) {
                const qreal newBase = clamp01(m_pointsN[0].y());
                if (!qFuzzyCompare(m_baselineN, newBase)) {
                    m_baselineN = newBase;
                    emit baselineNChanged();
                }
            }
        } else if (m_pointsN.size() == 1) {
            m_pointsN.clear();
            emit pointsNChanged();
        }

        resetGestureState();
        e->accept();
        return;
    }

    // line clicked => add point on the line
    if (isClick && m_pressedOnLine && m_draggedPointIndex < 0) {
        if (isNearLinePx(rel) && pointIndexAtPx(rel) < 0) {
            insertPointOnLineAtPx(rel);
        }
    }

    resetGestureState();
    e->accept();
}
