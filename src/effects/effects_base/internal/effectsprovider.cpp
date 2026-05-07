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

#include <map>

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

    // Providers must be available in ModuleManager for on-demand plugin loading.
    ModuleManager::Get().DiscoverProviders();

    // Register for future changes
    knownPluginsRegister()->pluginInfoListChanged().onNotify(this, [this]() {
        reloadEffects();
    });
}

void EffectsProvider::forgetPlugins(const EffectFilter& forget)
{
    doSave([&forget](const EffectMeta& meta) {
        return forget == nullptr || forget(meta);
    });
}

void EffectsProvider::rescanPlugins(muse::IInteractive& interactive,
                                    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
                                    const EffectFilter& exclude)
{
    if (doScanPlugins(registerAudioPluginsScenario, {}, exclude) == NewPluginsRegistered::No) {
        interactive.infoSync(muse::trc("audio", "Audio plugins scan completed"), muse::trc("audio", "All audio plugins are up to date."));
    }
}

EffectsProvider::NewPluginsRegistered EffectsProvider::doScanPlugins(
    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
    const std::function<bool()>& doScanThirdPartyPlugins,
    const EffectFilter& exclude)
{
    muse::audioplugins::PluginScanResult scanResult = registerAudioPluginsScenario.scanPlugins();

    muse::io::paths_t& thirdPartyPluginPaths = scanResult.newPluginPaths;
    const auto metaReaders = metaReaderRegister()->readers();

    std::map<muse::io::path_t, muse::audioplugins::IAudioPluginMetaReaderPtr> pathToMetaReader;
    {
        auto it = thirdPartyPluginPaths.begin();
        while (it != thirdPartyPluginPaths.end()) {
            const auto& path = *it;
            auto found = false;
            for (const auto& reader : metaReaders) {
                if (reader->canReadMeta(path)) {
                    pathToMetaReader[path] = reader;
                    found = true;
                    break;
                }
            }
            IF_ASSERT_FAILED(found) {
                it = thirdPartyPluginPaths.erase(it);
            } else {
                ++it;
            }
        }
    }

    if (exclude != nullptr) {
        // Erase plugins matching exclude
        auto it = thirdPartyPluginPaths.begin();
        while (it != thirdPartyPluginPaths.end()) {
            const auto& reader = pathToMetaReader.at(*it);
            const muse::RetVal<muse::audio::AudioResourceMetaList> ret = reader->readMeta(*it);
            IF_ASSERT_FAILED(ret.ret) {
                it = thirdPartyPluginPaths.erase(it);
                continue;
            }

            std::vector<EffectMeta> effectMetas;
            std::transform(ret.val.begin(), ret.val.end(), std::back_inserter(effectMetas), [path = *it](const auto& meta) {
                return utils::museToAuEffectMeta(path, meta);
            });
            // Only skip if all effects of this bundle are excluded - better be safe here.
            if (std::all_of(effectMetas.begin(), effectMetas.end(), exclude)) {
                it = thirdPartyPluginPaths.erase(it);
                continue;
            }

            // All good.
            ++it;
        }
    }

    // Audacity plugins (built-in effects and nyquist plugins) are safe. Register them in-process,
    // because out-of-process registration is slow and users may opt out. Remove them from the list of plugins.
    muse::io::paths_t audacityPluginPaths;
    auto it = thirdPartyPluginPaths.begin();
    while (it != thirdPartyPluginPaths.end()) {
        const auto& reader = pathToMetaReader.at(*it);
        const auto metaType = reader->metaType();

        using namespace muse::audio;
        const auto isAudacityPlugin = metaType == AudioResourceType::NyquistPlugin || metaType == AudioResourceType::NativeEffect;
        if (isAudacityPlugin) {
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

    reloadEffects();

    return !audacityPluginPaths.empty() || !thirdPartyPluginPaths.empty() ? NewPluginsRegistered::Yes : NewPluginsRegistered::No;
}

void EffectsProvider::deinit()
{
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();

    const auto knownPlugins = knownPluginsRegister()->pluginInfoList();
    std::transform(knownPlugins.begin(), knownPlugins.end(), std::back_inserter(m_effects),
                   [](const muse::audioplugins::AudioPluginInfo& info) {
        return utils::museToAuEffectMeta(info.path, info.meta, info.enabled);
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

bool EffectsProvider::paramsAreInputAgnostic(const EffectId& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta.paramsAreInputAgnostic;
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
    doSave();
}

void EffectsProvider::doSave(EffectFilter removeFromConfig)
{
    muse::audioplugins::AudioPluginInfoList newPlugins;

    for (const auto& meta : m_effects) {
        if (removeFromConfig != nullptr && removeFromConfig(meta)) {
            continue;
        }

        muse::audioplugins::AudioPluginInfo info;
        info.type = muse::audioplugins::AudioPluginType::Fx;
        info.meta = utils::auToMuseEffectMeta(meta);
        info.path = meta.path;
        info.enabled = meta.isLoadable;

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
