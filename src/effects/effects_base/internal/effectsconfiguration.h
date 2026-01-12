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
    muse::GlobalInject<muse::IGlobalConfiguration> globalConfiguration;

public:
    EffectsConfiguration() = default;

    void init();

    bool applyEffectToAllAudio() const override;
    void setApplyEffectToAllAudio(bool value) override;
    muse::async::Notification applyEffectToAllAudioChanged() const override;

    EffectMenuOrganization effectMenuOrganization() const override;
    void setEffectMenuOrganization(EffectMenuOrganization) override;
    muse::async::Notification effectMenuOrganizationChanged() const override;

    double previewMaxDuration() const override;
    void setPreviewMaxDuration(double value) override;

    EffectUIMode effectUIMode(const EffectId& effectId) const override;
    void setEffectUIMode(const EffectId& effectId, EffectUIMode mode) override;
    muse::async::Notification effectUIModeChanged() const override;

private:
    muse::async::Notification m_applyEffectToAllAudioChanged;
    muse::async::Notification m_effectMenuOrganizationChanged;
    muse::async::Notification m_effectUIModeChanged;
};
}
