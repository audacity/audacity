/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectloader.h"
#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/global/modularity/ioc.h"

class PluginProvider;

namespace au::effects {
class Au3EffectLoader : public IEffectLoader
{
public:
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    Au3EffectLoader(PluginProvider& provider, muse::audio::AudioResourceType resourceType);

    void init();

    EffectFamily family() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    virtual void doInit() {}

    PluginProvider& m_pluginProvider;
    const muse::audio::AudioResourceType m_resourceType;
};
} // namespace au::effects
