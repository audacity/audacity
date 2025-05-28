/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/effectstypes.h"
#include "audioplugins/iknownaudiopluginsregister.h"
#include "modularity/ioc.h"

class PluginProvider;

namespace au::effects {
class EffectsRepositoryHelper final : public muse::Injectable
{
public:
    muse::Inject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    EffectsRepositoryHelper(PluginProvider&, muse::audio::AudioResourceType);

    virtual ~EffectsRepositoryHelper() = default;

    EffectMetaList effectMetaList() const;
    bool ensurePluginIsLoaded(const EffectId& effectId) const;

private:
    PluginProvider& m_pluginProvider;
    const muse::audio::AudioResourceType m_resourceType;
};
} // namespace au::effects
