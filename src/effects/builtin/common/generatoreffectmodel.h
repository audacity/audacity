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
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged)

public:

    double duration() const;
    void setDuration(double newDuration);
    double sampleRate() const;
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);

signals:
    void sampleRateChanged();
    void durationChanged();
    void durationFormatChanged();

private:
    void doReload() final override;
    virtual void doEmitSignals() = 0;

    GeneratorEffect* generatorEffect() const;
};
} // namespace au::effects
