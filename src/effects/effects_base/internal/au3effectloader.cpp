/*
 * Audacity: A Digital Audio Editor
 */
#include "au3effectloader.h"
#include "au3-module-manager/PluginDescriptor.h"
#include "effectsutils.h"
#include "au3/au3effectsutils.h"

#include "au3-components/PluginProvider.h"
#include "au3-effects/Effect.h"
#include "au3-module-manager/ModuleManager.h"

#include "framework/global/io/path.h"
#include "framework/global/log.h"

namespace au::effects {
constexpr EffectFamily toEffectFamily(muse::audio::AudioResourceType type)
{
    switch (type) {
    case muse::audio::AudioResourceType::VstPlugin:      return EffectFamily::VST3;
#ifdef Q_OS_LINUX
    case muse::audio::AudioResourceType::Lv2Plugin:      return EffectFamily::LV2;
#endif
#ifdef Q_OS_MACOS
    case muse::audio::AudioResourceType::AudioUnit:      return EffectFamily::AudioUnit;
#endif
    case muse::audio::AudioResourceType::NyquistPlugin:  return EffectFamily::Nyquist;
    case muse::audio::AudioResourceType::NativeEffect:  return EffectFamily::Builtin;
    case muse::audio::AudioResourceType::FluidSoundfont:
    case muse::audio::AudioResourceType::MuseSamplerSoundPack:
        return EffectFamily::Unknown;
    default:
        assert(false && "Unknown AudioResourceType");
        return EffectFamily::Unknown;
    }
}

Au3EffectLoader::Au3EffectLoader(PluginProvider& provider, muse::audio::AudioResourceType resourceType)
    : m_pluginProvider{provider}, m_resourceType{resourceType}
{
    IF_ASSERT_FAILED(toEffectFamily(resourceType) != EffectFamily::Unknown) {
        LOGE() << "Invalid AudioResourceType for Au3EffectLoader: " << static_cast<int>(resourceType);
    }
}

void Au3EffectLoader::init()
{
    m_pluginProvider.Initialize();
    doInit();
}

void Au3EffectLoader::deinit()
{
    m_pluginProvider.Terminate();
}

EffectFamily Au3EffectLoader::family() const
{
    return toEffectFamily(m_resourceType);
}

bool Au3EffectLoader::ensurePluginIsLoaded(const EffectId& effectId)
{
    if (m_loadedInterfaces.find(effectId) != m_loadedInterfaces.end()) {
        // already loaded
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
    const muse::io::path_t& path = it->path;

    ::TranslatableString errorMessage{};
    ::PluginDescriptor desc;
    ::PluginPath au3path;

    // We need the complete effect's path, e.g. in VST a .vst3 bundle (one path) may contain several effects.
    // Hence an effect's UUID is appended to the .vst3 path for disambiguation.
    m_pluginProvider.DiscoverPluginsAtPath(
        path.toStdString(), errorMessage, [&](PluginProvider*, ComponentInterface* ident) -> const PluginID&
    {
        const auto effect = dynamic_cast<const EffectDefinitionInterface*>(ident);
        IF_ASSERT_FAILED(effect) {
            return desc.GetID();
        }
        if (effects::effectId(effect) == effectId.toStdString()) {
            au3path = ident->GetPath();
            desc.SetID(effectId.toStdString());
        }
        return desc.GetID();
    });

    if (au3path.empty()) {
        LOGE() << "Failed to find plugin path for effect: " << effectId;
        return false;
    }

    m_loadedInterfaces.emplace(effectId, m_pluginProvider.LoadPlugin(au3path));
    if (!m_loadedInterfaces.at(effectId)) {
        LOGE() << "Failed to load plugin: " << effectId;
        m_loadedInterfaces.erase(effectId);
        return false;
    }

    return true;
}

Effect* Au3EffectLoader::effect(const EffectId& effectId) const
{
    auto it = m_loadedInterfaces.find(effectId);
    if (it != m_loadedInterfaces.end()) {
        return dynamic_cast<Effect*>(it->second.get());
    }
    return nullptr;
}
} // namespace au::effects
