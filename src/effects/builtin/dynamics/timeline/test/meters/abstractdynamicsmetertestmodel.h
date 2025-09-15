#pragma once

#include <QObject>
#include <QTimer>

namespace au::effects {
class AbstractDynamicsMeterTestModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(double currentMax READ currentMax NOTIFY valueChanged)
    Q_PROPERTY(double globalMax READ globalMax NOTIFY valueChanged)
    Q_PROPERTY(double fiveSecMax READ fiveSecMax NOTIFY valueChanged)

public:
    explicit AbstractDynamicsMeterTestModel(QObject* parent = nullptr);

    double currentMax() const;
    double globalMax() const;
    double fiveSecMax() const;

signals:
    void valueChanged();

protected:
    enum class Direction {
        Upwards, Downwards
    };

private:
    virtual Direction direction() const = 0;
    void onTick();

    double m_currentMax = 0;
    double m_globalMax = 0;
    double m_fiveSecMax = 0;

    QTimer m_timer;
};
} // namespace au::effects
