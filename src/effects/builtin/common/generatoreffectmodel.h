/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

namespace au::effects {
class GeneratorEffect;
class ToneEffect;

class GeneratorEffectModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double duration READ duration WRITE setDuration NOTIFY durationChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE setDurationFormat NOTIFY durationFormatChanged)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY tempoChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY upperTimeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY lowerTimeSignatureChanged FINAL)

public:

    double duration() const;
    void setDuration(double newDuration);
    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);

signals:
    void sampleRateChanged();
    void tempoChanged();
    void upperTimeSignatureChanged();
    void lowerTimeSignatureChanged();
    void durationChanged();
    void durationFormatChanged();

private:
    void doReload() final override;
    virtual void doEmitSignals() = 0;

    GeneratorEffect* generatorEffect() const;
};
} // namespace au::effects
