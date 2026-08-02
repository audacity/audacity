/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectloader.h"

#include <utility>

#include "extensioneffect.h"
#include "extensioneffectsrepository.h"

namespace au::effects::extensions {
ExtensionEffectLoader::ExtensionEffectLoader(std::shared_ptr<ExtensionEffectsRepository> repository)
    : m_repository(std::move(repository))
{
}

ExtensionEffectLoader::~ExtensionEffectLoader() = default;

EffectFamily ExtensionEffectLoader::family() const
{
    return EffectFamily::Extension;
}

bool ExtensionEffectLoader::ensurePluginIsLoaded(const EffectId& effectId)
{
    if (m_effects.find(effectId) != m_effects.end()) {
        return true;
    }
    const auto* descriptor = m_repository->effect(effectId);
    if (!descriptor) {
        return false;
    }
    m_effects.emplace(effectId, std::make_unique<ExtensionEffect>(descriptor->descriptor));
    return true;
}

Effect* ExtensionEffectLoader::effect(const EffectId& effectId) const
{
    const auto found = m_effects.find(effectId);
    return found == m_effects.end() ? nullptr : found->second.get();
}

void ExtensionEffectLoader::retireAll()
{
    for (auto& effect : m_effects) {
        m_retired.push_back(std::move(effect.second));
    }
    m_effects.clear();
}

void ExtensionEffectLoader::deinit()
{
    m_effects.clear();
    m_retired.clear();
}
} // namespace au::effects::extensions
