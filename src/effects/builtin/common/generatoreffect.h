/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "modularity/ioc.h"

struct EffectSettings;

namespace au::effects {
class GeneratorEffect : public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::playback::IPlayback> playback;

public:
    GeneratorEffect(const double& t0, double& t1);

    //! `settings` remain valid for the lifetime of the effect
    void init(EffectSettings* settings);
    double sampleRate() const;
    double tempo() const;
    int upperTimeSignature() const;
    int lowerTimeSignature() const;
    double duration() const;
    void setDuration(double newDuration);
    QString durationFormat() const;
    void setDurationFormat(const QString& newDurationFormat);
    bool isApplyAllowed() const;

private:
    virtual void doInit() {}

    const double& m_t0;
    double& m_t1;
    EffectSettings* m_settings = nullptr;
};
}
