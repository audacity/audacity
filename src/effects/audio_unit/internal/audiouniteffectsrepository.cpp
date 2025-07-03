/*
* Audacity: A Digital Audio Editor
*/
#include "audiouniteffectsrepository.h"

au::effects::AudioUnitEffectsRepository::AudioUnitEffectsRepository()
    : m_helper(m_module, muse::audio::AudioResourceType::AudioUnit)
{
}

au::effects::EffectMetaList au::effects::AudioUnitEffectsRepository::effectMetaList() const
{
    return m_helper.effectMetaList();
}

bool au::effects::AudioUnitEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
