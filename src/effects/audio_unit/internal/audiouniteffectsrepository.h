/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iaudiouniteffectsrepository.h"

#include "audioplugins/iknownaudiopluginsregister.h"
#include "effects/effects_base/internal/effectsrepositoryhelper.h"
#include "libraries/lib-audio-unit/AudioUnitEffectsModule.h"

namespace au::effects {
class AudioUnitEffectsRepository : public IAudioUnitEffectsRepository
{
public:
    AudioUnitEffectsRepository();

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    ::AudioUnitEffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
