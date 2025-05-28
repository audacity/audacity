/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ivsteffectsrepository.h"
#include "effects/effects_base/internal/effectsrepositoryhelper.h"
#include "modularity/ioc.h"
#include "libraries/lib-vst3/VST3EffectsModule.h"

namespace au::effects {
class VstEffectsRepository final : public IVstEffectsRepository
{
public:
    VstEffectsRepository();

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId&) const override;

private:
    VST3EffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
