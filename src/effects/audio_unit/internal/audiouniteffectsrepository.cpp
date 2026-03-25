/*
* Audacity: A Digital Audio Editor
*/
#include "audiouniteffectsrepository.h"

au::effects::AudioUnitEffectsRepository::AudioUnitEffectsRepository()
    : m_helper{m_module, muse::audio::AudioResourceType::AudioUnit}
{
}

bool au::effects::AudioUnitEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
