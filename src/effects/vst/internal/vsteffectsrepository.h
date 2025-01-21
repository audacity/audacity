/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ivsteffectsrepository.h"

#include "modularity/ioc.h"
#include "audioplugins/iknownaudiopluginsregister.h"

namespace au::effects {
class VstEffectsRepository : public IVstEffectsRepository
{
    muse::Inject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;

public:
    VstEffectsRepository() = default;

    EffectMetaList effectMetaList() const override;

private:
    bool registerPlugin(const muse::io::path_t& path) const;
};
}
