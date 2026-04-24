/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "framework/audioplugins/iaudiopluginmetareaderregister.h"

#include "audioplugins/iknownaudiopluginsregister.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectloadersregister.h"

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
    muse::GlobalInject<muse::audioplugins::IAudioPluginMetaReaderRegister> metaReaderRegister;

public:
    void deinit();

    void initOnce(muse::IInteractive& interactive,
                  muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) override;
    muse::async::Notification initialized() const override;

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectMeta meta(const EffectId& effectId) const override;
    bool loadEffect(const EffectId& effectId) const override;
    std::string effectName(const std::string& effectId) const override;
    std::string effectName(const effects::RealtimeEffectState& state) const override;
    Effect* effect(const EffectId& effectId) const override;

    bool paramsAreInputAgnostic(const EffectId& effectId) const override;

private:
    void reloadEffects();
    IEffectLoaderPtr loader(const EffectId& effectId) const;

    muse::Ret doEffectPreview(EffectBase& effect, EffectSettings& settings);

    EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
    muse::async::Notification m_initialized;
};
}
