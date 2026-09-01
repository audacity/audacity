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

#include <optional>

#include <QCoreApplication>
#include <QEventLoop>

#include <map>
#include <set>

using namespace muse;
using namespace au::effects;

void EffectsProvider::initOnce(const muse::modularity::ContextPtr& ctx,
                               muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    m_registerAudioPluginsScenario = &registerAudioPluginsScenario;
    registerAudioPluginsScenario.pluginValidationFinished().onReceive(this, [this](const muse::io::path_t& pluginPath) {
        onPluginValidationFinished(pluginPath);
    });

    doScanPlugins(ctx, registerAudioPluginsScenario);

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

NewPluginsRegistered EffectsProvider::rescanPlugins(const muse::modularity::ContextPtr& ctx,
                                                    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
{
    return doScanPlugins(ctx, registerAudioPluginsScenario);
}

NewPluginsRegistered EffectsProvider::doScanPlugins(
    const muse::modularity::ContextPtr& ctx,
    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario)
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
        return !(family == EffectFamily::Nyquist || family == EffectFamily::Builtin || family == EffectFamily::Extension);
    });

    muse::io::paths_t trustedPluginPaths(mid, thirdPartyPluginPaths.end());
    thirdPartyPluginPaths.erase(mid, thirdPartyPluginPaths.end());

    knownPluginsRegister()->setPluginsState(scanResult.missingPluginPaths,
                                            muse::audioplugins::AudioPluginState::Missing);

    for (const io::path_t& path : trustedPluginPaths) {
        registerAudioPluginsScenario.registerPlugin(path);
    }

    if (!thirdPartyPluginPaths.empty()) {
        const muse::Ret ret = registerAudioPluginsScenario.registerNewPluginsAsync(thirdPartyPluginPaths);
        if (!ret) {
            LOGE() << "Failed to register new plugins: " << ret.toString();
        }
    }

    std::set<muse::io::path_t> obsoleteExtensionPaths;
    for (const auto& plugin : knownPluginsRegister()->pluginInfoList()) {
        if (plugin.state == muse::audioplugins::AudioPluginState::Missing
            && utils::effectFamilyFromCacheType(plugin.meta.type) == EffectFamily::Extension) {
            obsoleteExtensionPaths.insert(plugin.path);
        }
    }
    for (const auto& path : obsoleteExtensionPaths) {
        const muse::Ret result = knownPluginsRegister()->removePluginsAtPath(path);
        if (!result) {
            LOGE() << "Failed to remove obsolete extension effects at " << path << ": " << result.toString();
        }
    }

    reloadEffects();

    return !trustedPluginPaths.empty() || !thirdPartyPluginPaths.empty() || !obsoleteExtensionPaths.empty()
           ? NewPluginsRegistered::Yes : NewPluginsRegistered::No;
}

void EffectsProvider::deinit()
{
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();

    const auto knownPlugins = knownPluginsRegister()->pluginInfoList();
    std::transform(knownPlugins.begin(), knownPlugins.end(), std::back_inserter(m_effects),
                   [this](const muse::audioplugins::AudioPluginInfo& info) {
        EffectMeta effectMeta = utils::museToAuEffectMeta(info.path, info.meta, info.state);
        // Promote a known-good third-party plugin to NewlyValidated once it has been
        // re-validated in this session; until then it stays PreviouslyValidated.
        if (effectMeta.state == EffectState::PreviouslyValidated
            && m_registerAudioPluginsScenario
            && m_registerAudioPluginsScenario->isValidatedInSession(info.path)) {
            effectMeta.state = EffectState::NewlyValidated;
        }
        return effectMeta;
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

    if (needsFirstUseValidation(effectId)) {
        LOGE() << "Effect not yet validated: " << effectId;
        return false;
    }

    return loader->ensurePluginIsLoaded(effectId);
}

bool EffectsProvider::validateEffect(const muse::modularity::ContextPtr& ctx, const EffectId& effectId)
{
    const EffectMeta effectMeta = meta(effectId);
    if (!needsFirstUseValidation(effectMeta)) {
        return effectMeta.isLoadable();
    }

    // Shared with the continuation so that a completion arriving after we stop
    // waiting (e.g. the user cancelled) is a harmless no-op, not a dangling
    // reference.
    const auto success = std::make_shared<std::optional<bool> >();

    if (m_pendingValidations.count(effectMeta.path)) {
        // Validation already in progress: don't re-trigger it (validatePluginAsync
        // would dedup it away anyway), just append a continuation to be notified.
        m_pendingValidations.at(effectMeta.path).push_back([this, effectId, success]() {
            *success = meta(effectId).isLoadable();
        });
    } else {
        LOGI() << "Validating plugin before its first use in this session: " << effectMeta.id;
        validateEffectAsync(effectId).onResolve(this, [success](bool loadable) {
            *success = loadable;
        });
    }

    // Show a modal, cancellable progress dialog and pump the event loop until the
    // validation finishes (or the user cancels). The dialog is application-modal,
    // so the user cannot mutate project state meanwhile - which is why callers can
    // treat validate + apply as one synchronous step.
    au3::ProgressDialog dialog(ctx, muse::trc("effects", "Validating audio plugin"));
    dialog.start();

    while (!success->has_value() && !dialog.Cancelled()) {
        // WaitForMoreEvents blocks between events, so this doesn't spin the CPU.
        QCoreApplication::processEvents(QEventLoop::WaitForMoreEvents);
    }

    // On cancel the validation subprocess keeps running in the background; its
    // result is still recorded when it finishes (the continuation fires harmlessly).
    return success->value_or(false);
}

muse::async::Promise<bool> EffectsProvider::validateEffectAsync(const EffectId& effectId)
{
    return muse::async::make_promise<bool>([this, effectId](auto resolve) {
        const EffectMeta effectMeta = meta(effectId);
        if (!needsFirstUseValidation(effectMeta)) {
            return resolve(effectMeta.isLoadable());
        }

        IF_ASSERT_FAILED(m_registerAudioPluginsScenario) {
            return resolve(false);
        }

        LOGI() << "Validating plugin before its first use in this session: " << effectMeta.id;
        m_registerAudioPluginsScenario->validatePluginAsync(effectMeta.path);

        m_pendingValidations[effectMeta.path].push_back([this, effectId, resolve] {
            // resolved later, from onPluginValidationFinished: the body already
            // returned dummy_result(), so discard the Result token here
            (void)resolve(meta(effectId).isLoadable());
        });

        return muse::async::Promise<bool>::dummy_result();
    });
}

bool EffectsProvider::needsFirstUseValidation(const EffectId& id) const
{
    return needsFirstUseValidation(meta(id));
}

bool EffectsProvider::needsFirstUseValidation(const EffectMeta& effectMeta) const
{
    // The trusted-family and this-session logic now lives in the EffectState itself
    // (see effectStateFromRegister + the promotion in reloadEffects): an effect
    // still awaiting validation is PreviouslyValidated (known-good, lazy) or
    // Discovered (newly found, eager).
    return effectMeta.state == EffectState::PreviouslyValidated
           || effectMeta.state == EffectState::Discovered;
}

void EffectsProvider::onPluginValidationFinished(const muse::io::path_t& pluginPath)
{
    const auto it = m_pendingValidations.find(pluginPath);
    if (it == m_pendingValidations.end()) {
        return;
    }

    // detach first: in case a caller's resolve lambda invokes `validate(somePath)` again,
    // we'd be executing a lambda of this map while the map gets modified.
    const std::vector<std::function<void()> > continuations = std::move(it->second);
    m_pendingValidations.erase(it);

    for (const auto& continuation : continuations) {
        continuation();
    }
}

std::string EffectsProvider::effectPath(const std::string& effectId) const
{
    // Parse rather than querying meta in case the effect is missing
    return utils::parseEffectPath(muse::String::fromStdString(effectId));
}

std::string EffectsProvider::effectName(const std::string& effectId) const
{
    if (const auto meta = this->meta(EffectId::fromStdString(effectId)); meta.isValid()) {
        return utils::effectDisplayTitle(meta).toStdString();
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
        // Persist the register's view: Previously/NewlyValidated both save as Validated.
        info.state = effectStateToRegister(meta.state);

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
