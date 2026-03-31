/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3effectloader.h"
#include "spectrogram/ispectraleffectsregister.h"

#include "framework/global/modularity/ioc.h"

#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistEffectsRepository
{
    muse::GlobalInject<muse::audioplugins::IKnownAudioPluginsRegister> knownPlugins;
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;

public:
    NyquistEffectsRepository();

    void init();

private:
    EffectMetaList effectMetaList() const;

    // This member forces the linker to include LoadNyquist.cpp,
    // which contains DECLARE_BUILTIN_PROVIDER for the Nyquist module
    ::NyquistEffectsModule m_module;
    Au3EffectLoader m_loader;
};
}
