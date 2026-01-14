/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ivsteffectsrepository.h"
#include "effects/effects_base/internal/effectsrepositoryhelper.h"
#include "modularity/ioc.h"
#include "au3-vst3/VST3EffectsModule.h"

namespace au::effects {
class VstEffectsRepository final : public IVstEffectsRepository, public muse::Injectable
{
public:
    VstEffectsRepository(const muse::modularity::ContextPtr& ctx);

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId&) const override;

private:
    VST3EffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
