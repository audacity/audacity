/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsrepository.h"

using namespace au::effects;

VstEffectsRepository::VstEffectsRepository()
    : m_helper{m_module, muse::audio::AudioResourceType::VstPlugin}
{
}

EffectMetaList VstEffectsRepository::effectMetaList() const
{
    return m_helper.effectMetaList();
}

bool VstEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
