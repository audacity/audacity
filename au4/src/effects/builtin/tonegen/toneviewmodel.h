/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class ToneEffect;
class ToneViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double amplitude READ amplitude WRITE setAmplitude NOTIFY
               amplitudeChanged)
    Q_PROPERTY(double frequency READ frequency WRITE setFrequency NOTIFY
               frequencyChanged)
    Q_PROPERTY(
        int waveform READ waveform WRITE setWaveform NOTIFY waveformChanged)
    Q_PROPERTY(double duration READ duration WRITE setDuration NOTIFY
               durationChanged)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE setDurationFormat NOTIFY
               durationFormatChanged)

public:
    Q_INVOKABLE void init();

    double amplitude() const;
    void setAmplitude(double newAmplitude);
    double frequency() const;
    void setFrequency(double newFrequency);
    int waveform() const;
    void setWaveform(int newWaveform);
    double duration() const;
    void setDuration(double newDuration);
    double sampleRate() const;
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);

signals:
    void sampleRateChanged();
    void amplitudeChanged();
    void frequencyChanged();
    void waveformChanged();
    void durationChanged();
    void durationFormatChanged();

private:
    ToneEffect* effect() const;
};
}
