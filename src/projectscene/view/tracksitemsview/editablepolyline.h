#pragma once

#include <QQuickPaintedItem>
#include <QColor>
#include <QVector>
#include <QPointF>

// NOTE: all of fooN() function are normalized, returning 0..1 values

struct GhostPoint {
    QPointF point;
    qreal dist = 1e18;
};

class EditablePolyline : public QQuickPaintedItem
{
    Q_OBJECT

    Q_PROPERTY(QColor lineColor READ lineColor WRITE setLineColor NOTIFY lineColorChanged)
    Q_PROPERTY(qreal lineWidth READ lineWidth WRITE setLineWidth NOTIFY lineWidthChanged)
    Q_PROPERTY(qreal baselineN READ baselineN WRITE setBaselineN NOTIFY baselineNChanged)
    Q_PROPERTY(qreal pointRadius READ pointRadius WRITE setPointRadius NOTIFY pointRadiusChanged)
    Q_PROPERTY(qreal hitRadius READ hitRadius WRITE setHitRadius NOTIFY hitRadiusChanged)
    Q_PROPERTY(QVector<QPointF> pointsN READ pointsN WRITE setPointsN NOTIFY pointsNChanged)

public:
    explicit EditablePolyline(QQuickItem* parent = nullptr);

    QColor lineColor() const { return m_lineColor; }
    void setLineColor(const QColor&);

    qreal lineWidth() const { return m_lineWidth; }
    void setLineWidth(qreal);

    qreal baselineN() const { return m_baselineN; }
    void setBaselineN(qreal);

    qreal pointRadius() const { return m_pointRadius; }
    void setPointRadius(qreal);

    qreal hitRadius() const { return m_hitRadius; }
    void setHitRadius(qreal);

    QVector<QPointF> pointsN() const { return m_pointsN; }
    void setPointsN(const QVector<QPointF>&);

    void paint(QPainter* painter) override;

signals:
    void lineColorChanged();
    void lineWidthChanged();
    void baselineNChanged();
    void pointRadiusChanged();
    void hitRadiusChanged();
    void pointsNChanged();

protected:
    void hoverMoveEvent(QHoverEvent* e) override;
    void hoverLeaveEvent(QHoverEvent* e) override;
    void mousePressEvent(QMouseEvent* e) override;
    void mouseMoveEvent(QMouseEvent* e) override;
    void mouseReleaseEvent(QMouseEvent* e) override;

private:
    QVector<QPointF> polylinePx() const;
    bool isNearLinePx(const QPointF& px) const;
    int pointIndexAtPx(const QPointF& px) const;
    bool isInteractiveAtPx(const QPointF& px) const;
    int insertPointOnLineAtPx(const QPointF& px);
    qreal calculateLineYN(qreal xN) const;
    GhostPoint ghostPointToPolylinePx(const QPointF& px) const;

    void updateCursor();
    void resetGestureState();

    QPointF clamp01(const QPointF& p) const;
    qreal clamp01(qreal v) const;

private:
    QColor m_lineColor;
    qreal m_lineWidth = 1.0;
    qreal m_baselineN = 0.5;
    qreal m_pointRadius = 3.0;
    qreal m_hitRadius = 8.0;

    QVector<QPointF> m_pointsN;

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
    int m_draggedPointIndex = -1;
    QPointF m_pressSinglePointN;
};
