/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../inyquisteffectsrepository.h"
#include "effects/effects_base/internal/effectsrepositoryhelper.h"
#include "spectrogram/ispectraleffectsregister.h"

#include "framework/global/modularity/ioc.h"

#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistEffectsRepository : public INyquistEffectsRepository, public muse::Injectable
{
    muse::Inject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister { this };

public:
    NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx);

    void init();

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    // This member forces the linker to include LoadNyquist.cpp,
    // which contains DECLARE_BUILTIN_PROVIDER for the Nyquist module
    ::NyquistEffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
