/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"

namespace au::effects {
class ToneEffect;
class ToneViewModel : public GeneratorEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged)
    Q_PROPERTY(double amplitude READ amplitude WRITE prop_setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(double frequency READ frequency WRITE prop_setFrequency NOTIFY frequencyChanged)
    Q_PROPERTY(int waveform READ waveform WRITE prop_setWaveform NOTIFY waveformChanged)
    Q_PROPERTY(QList<QString> waveforms READ waveforms CONSTANT)

public:
    bool isApplyAllowed() const;
    QList<QString> waveforms() const;
    double amplitude() const;
    void prop_setAmplitude(double newAmplitude);
    double frequency() const;
    void prop_setFrequency(double newFrequency);
    int waveform() const;
    void prop_setWaveform(int newWaveform);

signals:
    void amplitudeChanged();
    void frequencyChanged();
    void waveformChanged();
    void isApplyAllowedChanged();

private:
    void doEmitSignals() override;

    ToneEffect* effect() const;
};
}
