/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

struct EffectSettings;

namespace au::effects {
class GeneratorEffect
{
public:
    GeneratorEffect(const double& projectRate, const double& t0, double& t1);

    //! `settings` remain valid for the lifetime of the effect
    void init(EffectSettings* settings);
    double sampleRate() const;
    double duration() const;
    void setDuration(double newDuration);
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);

private:
    virtual void doInit() {}

    const double& m_projectRate;
    const double& m_t0;
    double& m_t1;
    EffectSettings* m_settings = nullptr;
};
}
