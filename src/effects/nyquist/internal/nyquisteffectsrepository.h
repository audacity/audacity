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
class NyquistEffectsRepository : public INyquistEffectsRepository, public muse::Contextable
{
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;

public:
    NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx);

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    void registerSpectralEffects() const;

    // This member forces the linker to include LoadNyquist.cpp,
    // which contains DECLARE_BUILTIN_PROVIDER for the Nyquist module
    ::NyquistEffectsModule m_module;
    EffectsRepositoryHelper m_helper;
};
}
