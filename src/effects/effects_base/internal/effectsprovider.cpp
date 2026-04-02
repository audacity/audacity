/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"
#include "effectsutils.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-effects/Effect.h"
#include "au3-effects/EffectManager.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

#include "au3-module-manager/ModuleManager.h"

#include "framework/global/log.h"

using namespace muse;
using namespace au::effects;

void EffectsProvider::initOnce(muse::IInteractive& interactive,
                               muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    const auto doScanThirdPartyPlugins = [&interactive]() {
        auto ret = interactive.questionSync(muse::trc("appshell", "Scanning audio plugins"),
                                            muse::trc(
                                                "appshell",
                                                "Audacity has found plugins that need to be scanned before use. Would you like to scan them now or skip?"),
                                            { muse::IInteractive::ButtonData(
                                                  muse::IInteractive::Button::Cancel,
                                                  muse::trc("appshell", "Skip this time"),
                                                  false),
                                              muse::IInteractive::ButtonData(
                                                  muse::IInteractive::Button::Apply, muse::trc("appshell", "Scan plugins"),
                                                  true) },
                                            int(muse::IInteractive::Button::NoButton),
                                            {},
                                            muse::trc("appshell", "Audio plugin scan"));
        return ret.standardButton() == muse::IInteractive::Button::Apply;
    };

    doScanPlugins(registerAudioPluginsScenario, doScanThirdPartyPlugins);
}

void EffectsProvider::rescanPlugins(muse::IInteractive& interactive,
                                    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    if (!doScanPlugins(registerAudioPluginsScenario)) {
        interactive.infoSync(muse::trc("audio", "Audio plugins scan completed"), muse::trc("audio", "All audio plugins are up to date."));
    }
}

bool EffectsProvider::doScanPlugins(muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
                                    const std::function<bool()>& doScanThirdPartyPlugins)
{
    muse::audioplugins::PluginScanResult scanResult = registerAudioPluginsScenario.scanPlugins();

    // Audacity plugins (built-in effects and nyquist plugins) are safe. Register them in-process,
    // because out-of-process registration is slow and users may opt out.
    muse::io::paths_t& thirdPartyPluginPaths = scanResult.newPluginPaths;
    muse::io::paths_t audacityPluginPaths;
    auto it = thirdPartyPluginPaths.begin();
    while (it != thirdPartyPluginPaths.end()) {
        std::optional<bool> isAudacityPlugin;
        for (const auto& reader : metaReaderRegister()->readers()) {
            if (reader->canReadMeta(*it)) {
                using namespace muse::audio;
                const auto metaType = reader->metaType();
                isAudacityPlugin = metaType == AudioResourceType::NyquistPlugin || metaType == AudioResourceType::NativeEffect;
                break;
            }
        }
        assert(isAudacityPlugin.has_value());
        if (isAudacityPlugin.has_value() && *isAudacityPlugin) {
            audacityPluginPaths.push_back(*it);
            it = thirdPartyPluginPaths.erase(it);
        } else {
            ++it;
        }
    }

    registerAudioPluginsScenario.unregisterRemovedPlugins(scanResult.missingPluginIds);

    for (const io::path_t& path : audacityPluginPaths) {
        registerAudioPluginsScenario.registerPlugin(path);
    }

    if (!thirdPartyPluginPaths.empty() && (doScanThirdPartyPlugins == nullptr || doScanThirdPartyPlugins())) {
        registerAudioPluginsScenario.registerNewPlugins(thirdPartyPluginPaths);
    }

    // Providers must be available in ModuleManager for on-demand plugin loading.
    ModuleManager::Get().DiscoverProviders();

    reloadEffects();

    // Register for future changes
    knownPluginsRegister()->pluginInfoListChanged().onNotify(this, [this]() {
        reloadEffects();
    });

    return !thirdPartyPluginPaths.empty();
}

void EffectsProvider::deinit()
{
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();

    const auto knownPlugins = knownPluginsRegister()->pluginInfoList();
    std::for_each(knownPlugins.begin(), knownPlugins.end(),
                  [this](const muse::audioplugins::AudioPluginInfo& info) {
        if (info.enabled) {
            m_effects.push_back(utils::museToAuEffectMeta(info.path, info.meta));
        }
    });

    m_effectsChanged.notify();
}

EffectMetaList EffectsProvider::effectMetaList() const
{
    return m_effects;
}

muse::async::Notification EffectsProvider::effectMetaListChanged() const
{
    return m_effectsChanged;
}

EffectMeta EffectsProvider::meta(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return EffectMeta();
}

IEffectLoaderPtr EffectsProvider::loader(const EffectId& effectId) const
{
    const auto it = std::find_if(m_effects.begin(), m_effects.end(), [&](const EffectMeta& meta) {
        return meta.id == effectId;
    });
    if (it == m_effects.end()) {
        return nullptr;
    }
    return effectLoadersRegister()->loader(it->family);
}

bool EffectsProvider::loadEffect(const EffectId& effectId) const
{
    const IEffectLoaderPtr loader = this->loader(effectId);
    if (!loader) {
        return false;
    }
    return loader->ensurePluginIsLoaded(effectId);
}

std::string EffectsProvider::effectName(const std::string& effectId) const
{
    const auto it = std::find_if(m_effects.begin(), m_effects.end(), [&](const EffectMeta& meta) {
        return meta.id == effectId;
    });
    if (it == m_effects.end()) {
        return "";
    }
    return it->title.toStdString();
}

std::string EffectsProvider::effectName(const effects::RealtimeEffectState& state) const
{
    return effectName(state.GetID().ToStdString());
}

bool EffectsProvider::supportsMultipleClipSelection(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta.supportsMultipleClipSelection;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return false;
}

Effect* EffectsProvider::effect(const EffectId& effectId) const
{
    if (!loadEffect(effectId)) {
        return nullptr;
    }

    const IEffectLoaderPtr loader = this->loader(effectId);
    if (!loader) {
        return nullptr;
    }

    return loader->effect(effectId);
}

void EffectsProvider::setEffectActivated(const EffectId& effectId, bool activated)
{
    const auto it = std::find_if(m_effects.begin(), m_effects.end(), [&](const EffectMeta& meta) {
        return meta.id == effectId;
    });
    if (it == m_effects.end()) {
        LOGE() << "effect not found: " << effectId;
        return;
    }
    it->isActivated = activated;
    m_effectsChanged.notify();
}

void EffectsProvider::save()
{
    muse::audioplugins::AudioPluginInfoList newPlugins;
    newPlugins.reserve(m_effects.size());

    for (const auto& meta : m_effects) {
        muse::audioplugins::AudioPluginInfo info;
        info.type = muse::audioplugins::AudioPluginType::Fx;
        info.meta = utils::auToMuseEffectMeta(meta);
        info.path = meta.path;
        info.enabled = true;

        newPlugins.push_back(std::move(info));
    }

    const auto filePath = audioPluginsConfiguration()->knownAudioPluginsFilePath();
    if (fileSystem()->exists(filePath)) {
        // Remove the file and reload the register.
        fileSystem()->remove(filePath);
        knownPluginsRegister()->load();
    }

    knownPluginsRegister()->registerPlugins(newPlugins);
}
