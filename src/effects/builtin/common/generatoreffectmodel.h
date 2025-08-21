/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class GeneratorEffect;
class ToneEffect;

class GeneratorEffectModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(double duration READ duration WRITE prop_setDuration NOTIFY durationChanged)
    Q_PROPERTY(QString durationFormat READ durationFormat WRITE prop_setDurationFormat NOTIFY durationFormatChanged)
    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)
    Q_PROPERTY(double tempo READ tempo NOTIFY tempoChanged FINAL)
    Q_PROPERTY(int upperTimeSignature READ upperTimeSignature NOTIFY upperTimeSignatureChanged FINAL)
    Q_PROPERTY(int lowerTimeSignature READ lowerTimeSignature NOTIFY lowerTimeSignatureChanged FINAL)
    Q_PROPERTY(bool isApplyAllowed READ isApplyAllowed NOTIFY isApplyAllowedChanged FINAL)

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
        const T* st = settings().cast<T>();
        assert(st);
        return const_cast<T&>(*st);
    }

    EffectSettings& mutSettings()
    {
        return const_cast<EffectSettings&>(settings());
    }

private:
    void doReload() final override;
    virtual void doEmitSignals() = 0;
    void update();

    bool m_isApplyAllowed = false;
};
} // namespace au::effects
