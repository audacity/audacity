/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"
#include "dtmfgenerator.h"

namespace au::effects {
class DtmfGenerator;

class DtmfViewModel : public GeneratorEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString sequence READ sequence WRITE setSequence NOTIFY sequenceChanged)
    Q_PROPERTY(double amplitude READ amplitude WRITE setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(double dutyCycle READ dutyCycle WRITE setDutyCycle NOTIFY dutyCycleChanged)
    Q_PROPERTY(double toneDuration READ toneDuration NOTIFY toneDurationChanged)
    Q_PROPERTY(double silenceDuration READ silenceDuration NOTIFY silenceDurationChanged)

public:
    DtmfViewModel();
    virtual ~DtmfViewModel();

    QString sequence() const;
    void setSequence(const QString& newSequence);

    double amplitude() const;
    void setAmplitude(double newAmplitude);

    double dutyCycle() const;
    void setDutyCycle(double newDutyCycle);

    double toneDuration() const;
    double silenceDuration() const;

signals:
    void amplitudeChanged();
    void sequenceChanged();
    void dutyCycleChanged();
    void toneDurationChanged();
    void silenceDurationChanged();

private:

    void doReload() override {}

    DtmfSettings& mutSettings();
    const DtmfSettings& settings() const;

    void recalculateDurations();
};
}
