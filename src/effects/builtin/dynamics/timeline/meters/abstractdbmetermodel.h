#pragma once

#include "../abstractdynamicseffectinstancemodel.h"

#include "libraries/lib-dynamic-range-processor/MeterValueProvider.h"
#include "libraries/lib-utility/LockFreeQueue.h"

namespace au::effects {
class AbstractDbMeterModel : public AbstractDynamicsEffectInstanceModel
{
    Q_OBJECT

    Q_PROPERTY(double currentMax READ currentMax NOTIFY valueChanged)
    Q_PROPERTY(double globalMax READ globalMax NOTIFY valueChanged)
    Q_PROPERTY(double fiveSecMax READ fiveSecMax NOTIFY valueChanged)

public:
    explicit AbstractDbMeterModel(QObject* parent = nullptr);

    Q_INVOKABLE void update();

    double currentMax() const;
    double globalMax() const;
    double fiveSecMax() const;

signals:
    void valueChanged();

protected:
    enum class MeterType {
        OutputDb, CompressionDb
    };

    const std::shared_ptr<LockFreeQueue<float> > m_valueQueue;
    std::unique_ptr<MeterValueProvider> m_meter;

private:
    virtual float latestValue() = 0;

    double m_currentMax = 0;
    double m_globalMax = 0;
    double m_fiveSecMax = 0;
};
} // namespace au::effects
