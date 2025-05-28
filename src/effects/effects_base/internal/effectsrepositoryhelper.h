/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/effectstypes.h"
#include "audioplugins/iknownaudiopluginsregister.h"
#include "modularity/ioc.h"
#include "global/io/path.h"

class PluginProvider;

namespace au::effects {
class EffectsRepositoryHelper final : public muse::Injectable
{
public:
    muse::Inject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    using GetTitleFunc = std::function<muse::String (const muse::io::path_t&)>;

    EffectsRepositoryHelper(PluginProvider&, muse::audio::AudioResourceType, GetTitleFunc getTitle = nullptr);

    virtual ~EffectsRepositoryHelper() = default;

    EffectMetaList effectMetaList() const;
    bool ensurePluginIsLoaded(const EffectId& effectId) const;

private:
    PluginProvider& m_pluginProvider;
    const muse::audio::AudioResourceType m_resourceType;
    const GetTitleFunc m_getTitle;
};
} // namespace au::effects
