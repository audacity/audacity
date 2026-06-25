/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QPointF>
#include <QVector>

#include <optional>

namespace au::effects {
class FilterCurveEq;

class FilterCurveModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QVector<QPointF> points READ points NOTIFY pointsChanged FINAL)
    Q_PROPERTY(double defaultValue READ defaultValue CONSTANT FINAL)

public:
    FilterCurveModel(QObject* parent, FilterCurveEq& eq);

    void reload();

    QVector<QPointF> points() const;
    double defaultValue() const;

    Q_INVOKABLE void setPoint(int index, double x, double y, bool completed);
    Q_INVOKABLE void addPoint(double x, double y, bool completed);
    Q_INVOKABLE void removePoint(int index, bool completed);
    Q_INVOKABLE void cancelDrag();

    Q_INVOKABLE void flatten();
    Q_INVOKABLE void invert();

signals:
    void pointsChanged();

private:
    void rebuildFromEnvelope();
    void syncToEnvelope();
    void commitIfCompleted(bool completed);
    void beginDragIfNeeded();

    FilterCurveEq& m_eq;
    QVector<QPointF> m_points;
    std::optional<QVector<QPointF> > m_dragSnapshot;
};
}
