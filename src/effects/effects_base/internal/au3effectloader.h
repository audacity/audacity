/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/ieffectloader.h"

#include "au3-components/ComponentInterface.h"

#include "framework/audioplugins/iknownaudiopluginsregister.h"
#include "framework/global/modularity/ioc.h"

class PluginProvider;

namespace au::effects {
class Au3EffectLoader : public IEffectLoader
{
public:
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    Au3EffectLoader(PluginProvider& provider, EffectFamily family);

    void init();
    void deinit();

    EffectFamily family() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) override;
    Effect* effect(const EffectId& effectId) const override;

private:
    virtual void doInit() {}

    PluginProvider& m_pluginProvider;
    const EffectFamily m_family;
    std::map<EffectId, std::unique_ptr<::ComponentInterface> > m_loadedInterfaces;
};
} // namespace au::effects
