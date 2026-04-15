/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/effectstypes.h"
#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/io/path.h"

class PluginProvider;

namespace au::effects {
class EffectsRepositoryHelper final
{
public:
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    using GetTitleFunc = std::function<muse::String (const muse::io::path_t&)>;

    EffectsRepositoryHelper(PluginProvider&, muse::audio::AudioResourceType, GetTitleFunc getTitle = nullptr);

    ~EffectsRepositoryHelper() = default;

    EffectMetaList effectMetaList() const;
    bool ensurePluginIsLoaded(const EffectId& effectId) const;

private:
    PluginProvider& m_pluginProvider;
    const muse::audio::AudioResourceType m_resourceType;
    const GetTitleFunc m_getTitle;
};
} // namespace au::effects
