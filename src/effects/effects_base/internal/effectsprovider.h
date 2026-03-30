/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "audioplugins/iknownaudiopluginsregister.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectloadersregister.h"
#include "../ieffectviewlaunchregister.h"

#include "../ieffectsprovider.h"

class EffectBase;
class EffectSettingsAccess;
class TrackList;

namespace au::effects {
class EffectsProvider : public IEffectsProvider, public muse::async::Asyncable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPluginsRegister;
    muse::GlobalInject<IEffectLoadersRegister> effectLoadersRegister;

    muse::ContextInject<IEffectViewLaunchRegister> viewLaunchRegister{ this };

public:
    void init();

    void reloadEffects();

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectMeta meta(const EffectId& effectId) const override;
    bool loadEffect(const EffectId& effectId) const override;
    std::string effectName(const std::string& effectId) const override;
    std::string effectName(const effects::RealtimeEffectState& state) const override;
    std::string effectSymbol(const std::string& effectId) const override;
    Effect* effect(const EffectId& effectId) const override;

    bool supportsMultipleClipSelection(const EffectId& effectId) const override;

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;

    void showEffect(const RealtimeEffectStatePtr& state) const override;
    void hideEffect(const RealtimeEffectStatePtr& state) const override;

private:
    muse::Ret doEffectPreview(EffectBase& effect, EffectSettings& settings);

    mutable EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
};
}
