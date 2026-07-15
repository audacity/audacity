/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"
#include "effectsutils.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3wrap/internal/progressdialog.h"

#include "au3-effects/Effect.h"
#include "au3-effects/EffectManager.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

#include "au3-module-manager/ModuleManager.h"

#include "framework/global/log.h"
#include "stringutils.h"

#include <map>

using namespace muse;
using namespace au::effects;

void EffectsProvider::initOnce(const muse::modularity::ContextPtr& ctx, muse::IInteractive& interactive,
                               muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    const auto doScanThirdPartyPlugins = [&interactive]() {
        auto ret = interactive.questionSync(muse::trc("appshell", "Validate audio plugins"),
                                            muse::trc(
                                                "appshell",
                                                "Audacity has found plugins that need to be validated before use. Would you like to validate them now or skip?"),
                                            { muse::IInteractive::ButtonData(
                                                  muse::IInteractive::Button::Cancel,
                                                  muse::trc("appshell", "Skip this time"),
                                                  false),
                                              muse::IInteractive::ButtonData(
                                                  muse::IInteractive::Button::Apply, muse::trc("appshell", "Validate"),
                                                  true) },
                                            int(muse::IInteractive::Button::NoButton),
                                            {},
                                            muse::trc("appshell", "Audio plugin validation"));
        return ret.standardButton() == muse::IInteractive::Button::Apply;
    };

    doScanPlugins(ctx, registerAudioPluginsScenario, doScanThirdPartyPlugins);

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

void EffectsProvider::rescanPlugins(const muse::modularity::ContextPtr& ctx, muse::IInteractive& interactive,
                                    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    if (doScanPlugins(ctx, registerAudioPluginsScenario) == NewPluginsRegistered::No) {
        interactive.infoSync(muse::trc("audio", "Audio plugins scan completed"), muse::trc("audio", "All audio plugins are up to date."));
    }
}

EffectsProvider::NewPluginsRegistered EffectsProvider::doScanPlugins(
    const muse::modularity::ContextPtr& ctx,
    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
    const std::function<bool()>& doScanThirdPartyPlugins)
{
    muse::audioplugins::PluginScanResult scanResult;
    {
        au3::ProgressDialog progressDialog(ctx, muse::trc("audio", "Validating audio plugins"));
        // Scanners publish through `muse::Progress` directly (not via
        // Poll), so the QML dialog wouldn't mount on its own. Open it
        // explicitly before exposing the channel.
        progressDialog.start();
        scanResult = registerAudioPluginsScenario.scanPlugins(&progressDialog.museProgress());
    }

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

    const auto mid = std::stable_partition(
        thirdPartyPluginPaths.begin(), thirdPartyPluginPaths.end(),
        [&](const auto& path) {
        const auto family = utils::effectFamilyFromCacheType(pathToMetaReader.at(path)->metaType());
        return !(family == EffectFamily::Nyquist || family == EffectFamily::Builtin
                 || family == EffectFamily::AudacityPlugin);
    });

    muse::io::paths_t audacityPluginPaths(mid, thirdPartyPluginPaths.end());
    thirdPartyPluginPaths.erase(mid, thirdPartyPluginPaths.end());

    knownPluginsRegister()->setPluginsState(scanResult.missingPluginPaths,
                                            muse::audioplugins::AudioPluginState::Missing);

    // These plugin families are registered in-process.
    for (const io::path_t& path : audacityPluginPaths) {
        registerAudioPluginsScenario.registerPlugin(path);
    }

    if (!thirdPartyPluginPaths.empty()) {
        const bool validate = (doScanThirdPartyPlugins == nullptr || doScanThirdPartyPlugins());
        const muse::Ret ret = registerAudioPluginsScenario.registerNewPlugins(thirdPartyPluginPaths, validate);
        if (!ret) {
            LOGE() << "Failed to register new plugins: " << ret.toString();
        }
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
        return utils::museToAuEffectMeta(info.path, info.meta, info.state);
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

    // Here we don't log an error but an info, because metas may be queried for non-existent effects.
    LOGI() << "not found meta: " << effectId;
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

bool EffectsProvider::hasEffectFamily(EffectFamily family) const
{
    return effectLoadersRegister()->loader(family) != nullptr;
}

bool EffectsProvider::loadEffect(const EffectId& effectId) const
{
    const IEffectLoaderPtr loader = this->loader(effectId);
    if (!loader) {
        return false;
    }
    return loader->ensurePluginIsLoaded(effectId);
}

std::string EffectsProvider::effectPath(const std::string& effectId) const
{
    // Parse rather than querying meta in case the effect is missing
    return utils::parseEffectPath(muse::String::fromStdString(effectId));
}

std::string EffectsProvider::effectName(const std::string& effectId) const
{
    if (const auto meta = this->meta(EffectId::fromStdString(effectId)); meta.isValid()) {
        return meta.title.toStdString();
    }
    return utils::parseEffectName(muse::String::fromStdString(effectId));
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
        info.meta = utils::auToMuseEffectMeta(meta);
        info.path = meta.path;
        info.state = meta.state;

        newPlugins.push_back(std::move(info));
    }

    // registerPlugins() merges into the register; remove the cache and reload
    // so exactly newPlugins is persisted
    const auto filePath = audioPluginsConfiguration()->knownAudioPluginsFilePath();
    if (fileSystem()->exists(filePath)) {
        fileSystem()->remove(filePath);
    }
    knownPluginsRegister()->load();

    knownPluginsRegister()->registerPlugins(newPlugins);
}
