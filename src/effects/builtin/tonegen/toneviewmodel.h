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
    Q_PROPERTY(double amplitude READ amplitude WRITE setAmplitude NOTIFY amplitudeChanged)
    Q_PROPERTY(double frequency READ frequency WRITE setFrequency NOTIFY frequencyChanged)
    Q_PROPERTY(int waveform READ waveform WRITE setWaveform NOTIFY waveformChanged)
    Q_PROPERTY(QList<QString> waveforms READ waveforms CONSTANT)

public:
    bool isApplyAllowed() const;
    QList<QString> waveforms() const;
    double amplitude() const;
    void setAmplitude(double newAmplitude);
    double frequency() const;
    void setFrequency(double newFrequency);
    int waveform() const;
    void setWaveform(int newWaveform);

signals:
    void amplitudeChanged();
    void frequencyChanged();
    void waveformChanged();
    void isApplyAllowedChanged();

private:
    void doReload() override;

    ToneEffect* effect() const;
};
}
