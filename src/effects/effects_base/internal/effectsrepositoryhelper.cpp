/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsrepositoryhelper.h"

#include "libraries/lib-components/PluginProvider.h"
#include "libraries/lib-strings/TranslatableString.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "libraries/lib-module-manager/ModuleManager.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

namespace au::effects {
constexpr EffectFamily toEffectFamily(muse::audio::AudioResourceType type)
{
    switch (type) {
    case muse::audio::AudioResourceType::VstPlugin:
        return EffectFamily::VST3;
    case muse::audio::AudioResourceType::Lv2Plugin:
        return EffectFamily::LV2;
    case muse::audio::AudioResourceType::AudioUnit:
        return EffectFamily::AudioUnit;
    case muse::audio::AudioResourceType::FluidSoundfont:
    case muse::audio::AudioResourceType::MusePlugin:
    case muse::audio::AudioResourceType::MuseSamplerSoundPack:
        return EffectFamily::Unknown;
    default:
        assert(false && "Unknown AudioResourceType");
        return EffectFamily::Unknown;
    }
}

EffectsRepositoryHelper::EffectsRepositoryHelper(PluginProvider& provider, muse::audio::AudioResourceType resourceType,
                                                 GetTitleFunc getTitle)
    : m_pluginProvider{provider}, m_resourceType{resourceType}, m_getTitle{std::move(getTitle)}
{
    IF_ASSERT_FAILED(toEffectFamily(resourceType) != EffectFamily::Unknown) {
        LOGE() << "Invalid AudioResourceType for EffectsRepositoryHelper: " << static_cast<int>(resourceType);
    }
}

EffectMetaList EffectsRepositoryHelper::effectMetaList() const
{
    using namespace muse::audioplugins;
    using namespace muse::audio;

    EffectMetaList effects;

    const std::vector<AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

    for (const AudioPluginInfo& info : allEffects) {
        if (!(info.type == AudioPluginType::Fx && info.meta.type == m_resourceType)) {
            continue;
        }

        EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.family = toEffectFamily(m_resourceType);
        meta.title = m_getTitle ? m_getTitle(info.path) : muse::io::completeBasename(info.path).toString();
        meta.isRealtimeCapable = true;
        meta.vendor = muse::String::fromStdString(info.meta.vendor);
        meta.type = EffectType::Processor;
        meta.path = info.path;

        effects.push_back(std::move(meta));
    }

    return effects;
}

bool EffectsRepositoryHelper::ensurePluginIsLoaded(const EffectId& effectId) const
{
    if (PluginManager::Get().IsPluginLoaded(au3::wxFromString(effectId))) {
        return true;
    }

    const auto plugins = knownPlugins()->pluginInfoList();
    const auto it = std::find_if(plugins.begin(), plugins.end(), [&](const muse::audioplugins::AudioPluginInfo& info) {
        return info.meta.id == effectId.toStdString();
    });
    if (it == plugins.end()) {
        LOGE() << "plugin not in registry: " << effectId;
        return false;
    }
    if (!it->enabled) {
        LOGW() << "plugin disabled: " << effectId;
        return false;
    }
    if (it->meta.type != m_resourceType) {
        LOGE() << "Effect families don't match: expected " << static_cast<int>(m_resourceType)
               << ", got " << static_cast<int>(it->meta.type);
        return false;
    }
    const auto& path = it->path;

    PluginID pluginId;
    TranslatableString errorMessage{};
    int numPlugins = m_pluginProvider.DiscoverPluginsAtPath(
        au3::wxFromString(path.toString()), errorMessage,
        [&](PluginProvider* provider, ComponentInterface* ident) -> const PluginID&
    {
        pluginId = PluginManager::DefaultRegistrationCallback(provider, ident);
        return pluginId;
    });

    if (numPlugins == 0) {
        LOGE() << "not found plugin: " << path;
        return false;
    }

    const auto ptr = PluginManager::Get().GetPlugin(pluginId);
    if (!ptr) {
        LOGE() << "failed register plugin: " << au3::wxToStdSting(pluginId) << ", path: " << path;
        return false;
    }

    return true;
}
}
