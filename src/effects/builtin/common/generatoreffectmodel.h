/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class GeneratorEffect;
class ToneEffect;

class GeneratorEffectModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(
        double duration READ duration WRITE setDuration NOTIFY durationChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE setDurationFormat
               NOTIFY durationFormatChanged)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged)

public:
    Q_INVOKABLE void init();

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
    GeneratorEffect* generatorEffect() const;
};
} // namespace au::effects
