/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"
#include "effectsbridge.h"
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
                                    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
                                    const EffectFilter& exclude)
{
    if (doScanPlugins(ctx, registerAudioPluginsScenario, {}, exclude) == NewPluginsRegistered::No) {
        interactive.infoSync(muse::trc("audio", "Audio plugins scan completed"), muse::trc("audio", "All audio plugins are up to date."));
    }
}

EffectsProvider::NewPluginsRegistered EffectsProvider::doScanPlugins(
    const muse::modularity::ContextPtr& ctx,
    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario,
    const std::function<bool()>& doScanThirdPartyPlugins,
    const EffectFilter& exclude)
{
    muse::audioplugins::PluginScanResult scanResult;
    {
        au3::ProgressDialog progressDialog(ctx, muse::trc("audio", "Scanning audio plugins"));
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

    if (exclude != nullptr) {
        // Erase plugins matching exclude
        auto it = thirdPartyPluginPaths.begin();
        while (it != thirdPartyPluginPaths.end()) {
            const auto& reader = pathToMetaReader.at(*it);
            const muse::RetVal<muse::audioplugins::PluginMetaList> ret = reader->readMeta(*it);
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

        const EffectFamily family = fromWireString(metaType);
        const bool isAudacityPlugin = family == EffectFamily::Nyquist || family == EffectFamily::Builtin;
        if (isAudacityPlugin) {
            audacityPluginPaths.push_back(*it);
            it = thirdPartyPluginPaths.erase(it);
        } else {
            ++it;
        }
    }

    // Mark missing/rediscovered without deleting cache entries — the framework
    // wants to preserve the meta so the user can still see "the plugin I had
    // is gone". Mirrors RegisterAudioPluginsScenario::updatePluginsRegistry()
    // which MuseScore uses for the same purpose.
    knownPluginsRegister()->setPluginsState(scanResult.missingPluginIds,
                                            muse::audioplugins::AudioPluginState::Missing);
    knownPluginsRegister()->setPluginsState(scanResult.rediscoveredPluginIds,
                                            muse::audioplugins::AudioPluginState::Validated);

    for (const io::path_t& path : audacityPluginPaths) {
        registerAudioPluginsScenario.registerPlugin(path);
    }

    if (!thirdPartyPluginPaths.empty()) {
        // Skip ("validate later") still records the plugins as Discovered so
        // scanPlugins() can re-offer them on the next launch. Validate runs
        // the full out-of-process scan.
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

    const auto filePath = audioPluginsConfiguration()->knownAudioPluginsFilePath();
    if (fileSystem()->exists(filePath)) {
        // Remove the cache so the register starts clean.
        fileSystem()->remove(filePath);
    }
    // Always (re)load: on first launch the cache may not exist yet, but the
    // register still needs to be in the loaded state for registerPlugins
    // (it asserts m_loaded). load() handles the no-file case by initializing
    // an empty register.
    knownPluginsRegister()->load();

    knownPluginsRegister()->registerPlugins(newPlugins);
}
