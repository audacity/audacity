/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"

#include <functional>
#include <map>
#include <vector>
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
    muse::async::Promise<bool> validate(const EffectId& effectId) override;
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

    enum class ScanMode {
        // no dialogs; third-party plugins are validated in the background and
        // become available as their results arrive
        Background,
        // modal progress dialog, blocks until validation is complete
        Interactive,
    };

    NewPluginsRegistered doScanPlugins(const muse::modularity::ContextPtr& ctx,
                                       muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario, ScanMode scanMode);
    void doSave(EffectFilter removeFromConfig = nullptr);

    // Validate-on-first-use (#11746): a third-party plugin is validated in a
    // subprocess before its first in-process load in this session.
    bool needsFirstUseValidation(const EffectMeta& meta) const;
    void requestFirstUseValidation(const EffectMeta& meta) const;
    void onPluginValidationFinished(const muse::io::path_t& pluginPath);

    // set by initOnce; the app-wide scenario outlives this provider's use of it
    muse::audioplugins::IRegisterAudioPluginsScenario* m_registerAudioPluginsScenario = nullptr;
    std::map<muse::io::path_t, std::vector<std::function<void()> > > m_pendingValidations;

    EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;
};
}
