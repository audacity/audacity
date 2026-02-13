/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QColor>
#include <QPointF>
#include <QQuickPaintedItem>
#include <QVector>

#include "actions/actionable.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"

// NOTE: all of fooN() function are normalized, returning 0..1 values

struct GhostPoint {
    QPointF point;
    qreal distToSegment = 1e18;
};

class Polyline : public QQuickPaintedItem, public muse::async::Asyncable, public muse::actions::Actionable, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(QColor lineColor READ lineColor WRITE setLineColor NOTIFY lineColorChanged)
    Q_PROPERTY(qreal lineWidth READ lineWidth WRITE setLineWidth NOTIFY lineWidthChanged)
    Q_PROPERTY(qreal baselineN READ baselineN WRITE setBaselineN NOTIFY baselineNChanged)
    Q_PROPERTY(qreal pointRadius READ pointRadius WRITE setPointRadius NOTIFY pointRadiusChanged)
    Q_PROPERTY(qreal pointOutlineWidth READ pointOutlineWidth WRITE setPointOutlineWidth NOTIFY pointOutlineWidthChanged)
    Q_PROPERTY(QColor pointOutlineColor READ pointOutlineColor WRITE setPointOutlineColor NOTIFY pointOutlineColorChanged)
    Q_PROPERTY(
        QColor ghostPointOutlineColor READ ghostPointOutlineColor WRITE setGhostPointOutlineColor NOTIFY ghostPointOutlineColorChanged)
    Q_PROPERTY(qreal hitRadius READ hitRadius WRITE setHitRadius NOTIFY hitRadiusChanged)

    Q_PROPERTY(QVector<QPointF> points READ points WRITE setPoints NOTIFY pointsChanged)

    Q_PROPERTY(qreal defaultValue READ defaultValue WRITE setDefaultValue NOTIFY defaultValueChanged)

    Q_PROPERTY(qreal xRangeFrom READ xRangeFrom WRITE setXRangeFrom NOTIFY xRangeFromChanged)
    Q_PROPERTY(qreal xRangeTo READ xRangeTo WRITE setXRangeTo NOTIFY xRangeToChanged)

    Q_PROPERTY(qreal yRangeFrom READ yRangeFrom WRITE setYRangeFrom NOTIFY yRangeFromChanged)
    Q_PROPERTY(qreal yRangeTo READ yRangeTo WRITE setYRangeTo NOTIFY yRangeToChanged)

    // TODO: dB or linear (separate setting for x and y axis)

    Q_PROPERTY(bool yAxisInverse READ yAxisInverse WRITE setYAxisInverse NOTIFY yAxisInverseChanged)

    Q_PROPERTY(qreal dragX READ dragX WRITE setDragX NOTIFY dragXChanged)
    Q_PROPERTY(qreal dragY READ dragY WRITE setDragY NOTIFY dragYChanged)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    explicit Polyline(QQuickItem* parent = nullptr);

    Q_INVOKABLE void init();

    QColor lineColor() const;
    void setLineColor(const QColor&);

    qreal lineWidth() const;
    void setLineWidth(qreal);

    qreal baselineN() const;
    void setBaselineN(qreal);

    qreal pointRadius() const;
    void setPointRadius(qreal);

    qreal pointOutlineWidth() const;
    void setPointOutlineWidth(qreal);

    QColor pointOutlineColor() const;
    void setPointOutlineColor(const QColor&);

    QColor ghostPointOutlineColor() const;
    void setGhostPointOutlineColor(const QColor&);

    qreal hitRadius() const;
    void setHitRadius(qreal);

    QVector<QPointF> points() const;
    void setPoints(const QVector<QPointF>&);

    qreal defaultValue() const;
    void setDefaultValue(qreal v);

    qreal xRangeFrom() const;
    void setXRangeFrom(qreal);

    qreal xRangeTo() const;
    void setXRangeTo(qreal);

    qreal yRangeFrom() const;
    void setYRangeFrom(qreal);

    qreal yRangeTo() const;
    void setYRangeTo(qreal);

    bool yAxisInverse() const;
    void setYAxisInverse(bool);

    qreal dragX() const;
    void setDragX(qreal);

    qreal dragY() const;
    void setDragY(qreal);

    void geometryChange(const QRectF& newG, const QRectF& oldG) override;
    void paint(QPainter* painter) override;

signals:
    void lineColorChanged();
    void lineWidthChanged();
    void baselineNChanged();
    void pointRadiusChanged();
    void pointOutlineWidthChanged();
    void pointOutlineColorChanged();
    void ghostPointOutlineColorChanged();
    void hitRadiusChanged();

    void polylineFlattenRequested(qreal y, bool completed);
    void pointAdded(qreal x, qreal y, bool completed);
    void pointMoved(int index, qreal x, qreal y, bool completed);
    void pointRemoved(int index, bool completed);
    void dragCancelled();
    void interactionFinished();

    void xRangeFromChanged();
    void xRangeToChanged();
    void yRangeFromChanged();
    void yRangeToChanged();
    void yAxisInverseChanged();

    void defaultValueChanged();
    void pointsNChanged();
    void pointsChanged();

    void dragXChanged();
    void dragYChanged();

protected:
    void hoverMoveEvent(QHoverEvent* e) override;
    void hoverLeaveEvent(QHoverEvent* e) override;
    void mousePressEvent(QMouseEvent* e) override;
    void mouseMoveEvent(QMouseEvent* e) override;
    void mouseReleaseEvent(QMouseEvent* e) override;

private:
    QVector<QPointF> polylinePx() const;
    bool isNearLinePx(const QPointF& px) const;
    GhostPoint ghostPointToPolylinePx(const QPointF& px) const;
    int pointIndexAtPx(const QPointF& px) const;

    void updateCursor();
    void resetGestureState();

    qreal clamp01(qreal v) const;
    QPointF clamp01(const QPointF& p) const;

    void rebuildVisiblePoints();
    QVector<QPointF> normalizedFromDomain(const QVector<QPointF>& pts) const;
    QVector<QPointF> domainFromNormalized(const QVector<QPointF>& ptsN) const;

    QPointF domainFromNormalized(const QPointF& pN) const;
    QPointF normalizedFromDomain(const QPointF& p) const;

    bool hasValidXRange() const;
    bool hasValidYRange() const;

private:
    QColor m_lineColor;
    qreal m_lineWidth = 1.0;
    qreal m_baselineN =0.5;
    qreal m_pointRadius = 3.0;
    qreal m_pointOutlineWidth = 1.0;
    QColor m_pointOutlineColor;
    QColor m_ghostPointOutlineColor;
    qreal m_hitRadius = 8.0;

    QVector<QPointF> m_points;          // domain points as provided from model
    QVector<QPointF> m_pointsNVisible;  // normalized points [0..1], cropped to frame boundaries (used for drawing only)

    // mapping for m_pointsNVisible -> index in m_points
    QVector<int> m_visibleToDomainIndex;

    qreal m_defaultValue = 1.0;

    qreal m_xFrom = 0.0;
    qreal m_xTo   = 1.0;
    qreal m_yFrom = 0.0;
    qreal m_yTo   = 1.0;
    bool m_yAxisInverse = true;

    bool m_hoveredOnLine = false;
    QPointF m_hoverPx;
    QPointF m_hoverGhostPx;

    bool m_pressedOnLine = false;
    bool m_pressed = false;
    QPointF m_pressPx;
    qreal m_pressBaselineN = 0.5;
    bool m_pressedOnPoint = false;
    int m_pressedPointIndex = -1;

    bool m_draggingLine = false;
    bool m_movedSincePress = false;

    // used for tracking x/y position during drag
    qreal m_dragX = 0.0;
    qreal m_dragY = 0.0;
};
