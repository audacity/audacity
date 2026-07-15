/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <map>
#include <memory>

#include "effects/effects_base/ieffectloader.h"
#include "framework/global/modularity/ioc.h"

#include "audacityplugin/iaudacitypluginhost.h"

namespace au::effects {
class AudacityPluginEffect;

class AudacityPluginEffectLoader final : public IEffectLoader
{
    muse::GlobalInject<au::audacityplugin::IAudacityPluginHost> audacityPluginHost;

public:
    AudacityPluginEffectLoader();
    ~AudacityPluginEffectLoader() override;

    EffectFamily family() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) override;
    Effect* effect(const EffectId& effectId) const override;
    void deinit();

private:
    std::map<EffectId, std::unique_ptr<AudacityPluginEffect> > m_effects;
};
} // namespace au::effects
