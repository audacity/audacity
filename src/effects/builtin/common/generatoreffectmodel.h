/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class GeneratorEffect;
class ToneEffect;

class GeneratorEffectModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double duration READ duration WRITE prop_setDuration NOTIFY durationChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE prop_setDurationFormat NOTIFY durationFormatChanged)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY tempoChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY upperTimeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY lowerTimeSignatureChanged FINAL)
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:

    double duration() const;
    void prop_setDuration(double newDuration);
    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;
    QString durationFormat() const;
    void prop_setDurationFormat(const QString& newDurationFormat);
    virtual bool isApplyAllowed() const;

signals:
    void sampleRateChanged();
    void tempoChanged();
    void upperTimeSignatureChanged();
    void lowerTimeSignatureChanged();
    void durationChanged();
    void durationFormatChanged();
    void isApplyAllowedChanged();

protected:
    template<typename T>
    T& mutSettings()
    {
        // Generators have singleton usage ; no risk of concurrency, settings may be returned without protection.
        EffectSettings* s = const_cast<EffectSettings*>(this->settings());
        assert(s);
        if (!s) {
            static T null;
            return null;
        }
        T* st = s->cast<T>();
        assert(st);
        return *st;
    }

    EffectSettings& mutSettings()
    {
        return *const_cast<EffectSettings*>(this->settings());
    }

private:
    void doReload() final override;
    virtual void doEmitSignals() = 0;
    void update();

    GeneratorEffect* generatorEffect() const;

    bool m_isApplyAllowed = false;
};
} // namespace au::effects
