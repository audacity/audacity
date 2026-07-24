/*
 * Audacity: A Digital Audio Editor
 */
#include "audacityplugineffectloader.h"

#include <algorithm>

#include "audacityplugineffect.h"
#include "audacitypluginids.h"

namespace au::effects {
AudacityPluginEffectLoader::AudacityPluginEffectLoader() = default;

AudacityPluginEffectLoader::~AudacityPluginEffectLoader() = default;

EffectFamily AudacityPluginEffectLoader::family() const
{
    return EffectFamily::AudacityPlugin;
}

bool AudacityPluginEffectLoader::ensurePluginIsLoaded(const EffectId& effectId)
{
    if (m_effects.find(effectId) != m_effects.end()) {
        return true;
    }
    if (!audacityPluginHost()) {
        return false;
    }

    const auto& descriptors = audacityPluginHost()->effects();
    const auto found = std::find_if(descriptors.begin(), descriptors.end(), [&](const auto& descriptor) {
        return audacity_plugin::makeEffectId(descriptor) == effectId;
    });
    if (found == descriptors.end()) {
        return false;
    }

    m_effects.emplace(effectId, std::make_unique<AudacityPluginEffect>(*found));
    return true;
}

Effect* AudacityPluginEffectLoader::effect(const EffectId& effectId) const
{
    const auto found = m_effects.find(effectId);
    return found == m_effects.end() ? nullptr : found->second.get();
}

void AudacityPluginEffectLoader::deinit()
{
    m_effects.clear();
}
} // namespace au::effects
