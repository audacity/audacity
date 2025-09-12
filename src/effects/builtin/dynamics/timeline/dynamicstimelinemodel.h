#pragma once

#include <QObject>
#include <QTimer>

#include <unordered_map>

namespace au::effects {
class DynamicsTimelineModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY
               isClippingChanged)
    Q_PROPERTY(double samplePeriod READ samplePeriod CONSTANT)

public:
    explicit DynamicsTimelineModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool clipping);
    double samplePeriod() const;

signals:
    void isClippingChanged();
    void newSample(double inputDb, double outputDb, double compressionDb);
    void tChanged();

private:
    void addDataPoint();

    bool m_isClipping = false;

    QTimer* m_sampleTimer = nullptr;
    QTimer* m_clippingTimer = nullptr;
    int m_sampleCount = 0;
};
} // namespace au::effects
