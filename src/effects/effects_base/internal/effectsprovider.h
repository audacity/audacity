/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "framework/audioplugins/iaudiopluginmetareaderregister.h"
#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/audioplugins/iaudiopluginsconfiguration.h"
#include "framework/global/io/ifilesystem.h"

#include "../ieffectsconfiguration.h"
#include "../ieffectloadersregister.h"

#include "../ieffectsprovider.h"

#include "au3-utility/Observer.h"

class EffectBase;
class EffectSettingsAccess;
class TrackList;

namespace au::effects {
class EffectsProvider : public IEffectsProvider, public muse::async::Asyncable
{
    muse::GlobalInject<IEffectsConfiguration> configuration;
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPluginsRegister;
    muse::GlobalInject<muse::audioplugins::IAudioPluginMetaReaderRegister> metaReaderRegister;
    muse::GlobalInject<muse::audioplugins::IAudioPluginsConfiguration> audioPluginsConfiguration;
    muse::GlobalInject<IEffectLoadersRegister> effectLoadersRegister;
    muse::GlobalInject<muse::io::IFileSystem> fileSystem;

public:
    void deinit();

    void initOnce(const muse::modularity::ContextPtr& ctx, muse::IInteractive& interactive,
                  muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) override;

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectMeta meta(const EffectId& effectId) const override;
    bool loadEffect(const EffectId& effectId) const override;
    std::string effectPath(const std::string& effectId) const override;
    std::string effectName(const std::string& effectId) const override;
    std::string effectName(const effects::RealtimeEffectState& state) const override;
    Effect* effect(const EffectId& effectId) const override;
    void setEffectActivated(const EffectId& effectId, bool activated) override;

    bool paramsAreInputAgnostic(const EffectId& effectId) const override;

    bool hasEffectFamily(EffectFamily family) const override;

    void rescanPlugins(const muse::modularity::ContextPtr& ctx, muse::IInteractive& interactive,
                       muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) override;
    void forgetPlugins(const EffectFilter& forget = nullptr) override;
    void save() override;

private:
    void reloadEffects();
    IEffectLoaderPtr loader(const EffectId& effectId) const;

    enum NewPluginsRegistered {
        Yes,
        No,
    };

    NewPluginsRegistered doScanPlugins(const muse::modularity::ContextPtr& ctx,
                                       muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
                                       const std::function<bool()>& doScanThirdPartyPlugins = nullptr);
    void doSave(EffectFilter removeFromConfig = nullptr);

    EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
};
}
