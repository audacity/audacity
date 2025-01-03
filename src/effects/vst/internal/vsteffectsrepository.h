/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ivsteffectsrepository.h"

#include "audioplugins/iknownaudiopluginsregister.h"
#include "modularity/ioc.h"

namespace au::effects {
class VstEffectsRepository : public IVstEffectsRepository
{
    muse::Inject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    VstEffectsRepository() = default;

    EffectMetaList effectMetaList() const override;
};
}  // namespace au::effects
