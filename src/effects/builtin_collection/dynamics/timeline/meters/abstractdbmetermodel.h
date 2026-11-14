/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../abstractdynamicseffectinstancemodel.h"
#include "../stopwatch.h"

#include "au3-dynamic-range-processor/MeterValueProvider.h"
#include "au3-utility/LockFreeQueue.h"

namespace au::effects {
class AbstractDbMeterModel : public AbstractDynamicsEffectInstanceModel
{
    Q_OBJECT

    Q_PROPERTY(au::effects::Stopwatch::PlayState playState READ playState WRITE setPlaystate NOTIFY playStateChanged)
    Q_PROPERTY(double currentMax READ currentMax NOTIFY valueChanged)
    Q_PROPERTY(double globalMax READ globalMax NOTIFY valueChanged)
    Q_PROPERTY(double fiveSecMax READ fiveSecMax NOTIFY valueChanged)

public:
    explicit AbstractDbMeterModel(MeterValueProvider::Direction, QObject* parent = nullptr);

    Q_INVOKABLE void update();
    Q_INVOKABLE void reset();

    Stopwatch::PlayState playState() const;
    void setPlaystate(Stopwatch::PlayState state);

    double currentMax() const;
    double globalMax() const;
    double fiveSecMax() const;

signals:
    void valueChanged();
    void playStateChanged();

private:
    virtual float latestValue() = 0;
    void doInit() final override;

    Stopwatch::PlayState m_playState = Stopwatch::Stopped;

protected:
    const std::shared_ptr<LockFreeQueue<float> > m_valueQueue;
    std::unique_ptr<MeterValueProvider> m_meter;
};
} // namespace au::effects
