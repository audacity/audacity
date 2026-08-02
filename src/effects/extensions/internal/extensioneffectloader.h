/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <map>
#include <memory>
#include <vector>

#include "effects/effects_base/ieffectloader.h"

namespace au::effects::extensions {
class ExtensionEffect;
class ExtensionEffectsRepository;

class ExtensionEffectLoader final : public IEffectLoader
{
public:
    explicit ExtensionEffectLoader(std::shared_ptr<ExtensionEffectsRepository> repository);
    ~ExtensionEffectLoader() override;

    EffectFamily family() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) override;
    Effect* effect(const EffectId& effectId) const override;
    void retireAll();
    void deinit();

private:
    std::shared_ptr<ExtensionEffectsRepository> m_repository;
    std::map<EffectId, std::unique_ptr<ExtensionEffect> > m_effects;
    std::vector<std::unique_ptr<ExtensionEffect> > m_retired;
};
} // namespace au::effects::extensions
