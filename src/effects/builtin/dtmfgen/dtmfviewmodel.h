/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"

namespace au::effects {
class DtmfGenerator;

class DtmfViewModel : public GeneratorEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString sequence READ sequence WRITE prop_setSequence NOTIFY sequenceChanged)
    Q_PROPERTY(double amplitude READ amplitude WRITE prop_setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(double dutyCycle READ dutyCycle WRITE prop_setDutyCycle NOTIFY dutyCycleChanged)
    Q_PROPERTY(double toneDuration READ toneDuration NOTIFY toneDurationChanged)
    Q_PROPERTY(double silenceDuration READ silenceDuration NOTIFY silenceDurationChanged)

public:
    DtmfViewModel();
    virtual ~DtmfViewModel();

    bool isApplyAllowed() const override;

    QString sequence() const;
    void prop_setSequence(const QString& newSequence);

    double amplitude() const;
    void prop_setAmplitude(double newAmplitude);

    double dutyCycle() const;
    void prop_setDutyCycle(double newDutyCycle);

    double toneDuration() const;
    double silenceDuration() const;

signals:
    void amplitudeChanged();
    void sequenceChanged();
    void dutyCycleChanged();
    void toneDurationChanged();
    void silenceDurationChanged();

private:

    void doEmitSignals() override;

    void recalculateDurations();
};
}
