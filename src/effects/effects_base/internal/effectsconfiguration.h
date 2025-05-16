/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectsconfiguration.h"

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

namespace au::effects {
class EffectsConfiguration : public IEffectsConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    EffectsConfiguration() = default;

    void init();

    bool applyEffectToAllAudio() const override;
    void setApplyEffectToAllAudio(bool value) override;
    muse::async::Notification applyEffectToAllAudioChanged() const override;

    EffectMenuOrganization effectMenuOrganization() const override;
    void setEffectMenuOrganization(EffectMenuOrganization) override;
    muse::async::Notification effectMenuOrganizationChanged() const override;

private:
    muse::async::Notification m_applyEffectToAllAudioChanged;
    muse::async::Notification m_effectMenuOrganizationChanged;
};
}
