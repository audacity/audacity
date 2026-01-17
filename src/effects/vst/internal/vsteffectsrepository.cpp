/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsrepository.h"

using namespace au::effects;

VstEffectsRepository::VstEffectsRepository(const muse::modularity::ContextPtr& ctx)
    : muse::Injectable(ctx), m_helper{ctx, m_module, muse::audio::AudioResourceType::VstPlugin}
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
