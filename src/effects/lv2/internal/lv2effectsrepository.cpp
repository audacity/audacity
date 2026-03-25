/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2effectsrepository.h"

namespace au::effects {
Lv2EffectsRepository::Lv2EffectsRepository()
    : m_helper{m_module, muse::audio::AudioResourceType::Lv2Plugin}
{
}

bool Lv2EffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
}
