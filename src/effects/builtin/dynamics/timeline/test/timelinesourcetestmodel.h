/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QTimer>

#include <unordered_map>

namespace au::effects {
class TimelineSourceTestModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged)
    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY
               isClippingChanged)
    Q_PROPERTY(double dataPointRate READ dataPointRate CONSTANT)
    Q_PROPERTY(double latency READ latency CONSTANT)

public:
    explicit TimelineSourceTestModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int instanceId() const { return m_instanceId; }
    void setInstanceId(int id);
    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool clipping);
    double dataPointRate() const;
    double latency() const;

signals:
    void isClippingChanged();
    void newSamples(const QVariantList& samples);
    void tChanged();
    void instanceIdChanged();

private:
    void addDataPoints();

    bool m_isClipping = false;
    int m_instanceId = -1;

    QTimer* m_deliveryTimer = nullptr;
    QTimer* m_clippingTimer = nullptr;
    int m_dataPointCount = 0;
};
} // namespace au::effects
