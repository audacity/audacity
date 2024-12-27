/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class ToneEffect;
class ToneViewModel : public GeneratorEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double amplitudeStart READ amplitudeStart WRITE prop_setAmplitudeStart NOTIFY amplitudeStartChanged)
    Q_PROPERTY(double amplitudeEnd READ amplitudeEnd WRITE prop_setAmplitudeEnd NOTIFY amplitudeEndChanged)
    Q_PROPERTY(double frequencyStart READ frequencyStart WRITE prop_setFrequencyStart NOTIFY frequencyStartChanged)
    Q_PROPERTY(double frequencyEnd READ frequencyEnd WRITE prop_setFrequencyEnd NOTIFY frequencyEndChanged)
    Q_PROPERTY(int waveform READ waveform WRITE prop_setWaveform NOTIFY waveformChanged)
    Q_PROPERTY(int interpolation READ interpolation WRITE prop_setInterpolation NOTIFY interpolationChanged)
    Q_PROPERTY(QList<QString> waveforms READ waveforms CONSTANT)
    Q_PROPERTY(QList<QString> interpolationTypes READ interpolationTypes CONSTANT)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    bool isApplyAllowed() const override;
    QList<QString> waveforms() const;
    QList<QString> interpolationTypes() const;
    double amplitudeStart() const;
    void prop_setAmplitudeStart(double newAmplitude);
    double amplitudeEnd() const;
    void prop_setAmplitudeEnd(double newAmplitude);
    double frequencyStart() const;
    void prop_setFrequencyStart(double newFrequency);
    double frequencyEnd() const;
    void prop_setFrequencyEnd(double newFrequency);
    int waveform() const;
    void prop_setWaveform(int newWaveform);
    int interpolation() const;
    void prop_setInterpolation(int newInterpolation);

signals:
    void amplitudeStartChanged();
    void amplitudeEndChanged();
    void frequencyStartChanged();
    void frequencyEndChanged();
    void waveformChanged();
    void interpolationChanged();

private:
    void doEmitSignals() override;

    ToneEffect* effect() const;
};
}
