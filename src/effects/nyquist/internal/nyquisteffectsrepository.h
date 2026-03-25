/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../inyquisteffectsrepository.h"
#include "spectrogram/ispectraleffectsregister.h"

#include "framework/global/modularity/ioc.h"
#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/audioplugins/iaudiopluginmetareader.h"

#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistEffectsRepository : public INyquistEffectsRepository, public muse::Contextable
{
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;

public:
    NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx,
                             std::unique_ptr<muse::audioplugins::IAudioPluginsScanner> nyquistPluginScanner,
                             std::shared_ptr<muse::audioplugins::IAudioPluginMetaReader> nyquistPluginMetaReader);

    void init();

    EffectMetaList effectMetaList() const override;
    bool ensurePluginIsLoaded(const EffectId& effectId) const override;

private:
    // This member forces the linker to include LoadNyquist.cpp,
    // which contains DECLARE_BUILTIN_PROVIDER for the Nyquist module
    ::NyquistEffectsModule m_module;
    const std::unique_ptr<muse::audioplugins::IAudioPluginsScanner> m_nyquistPluginScanner;
    const std::shared_ptr<muse::audioplugins::IAudioPluginMetaReader> m_nyquistPluginMetaReader;
};
}
