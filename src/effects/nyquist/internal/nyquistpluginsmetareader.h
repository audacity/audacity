/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/abstractaudiopluginmetareader.h"

#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistPluginsMetaReader : public AbstractAudioPluginMetaReader
{
public:
    NyquistPluginsMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;
    muse::RetVal<muse::audio::AudioResourceMetaList> readMeta(const muse::io::path_t& pluginPath) const override;

protected:
    void doInit(const muse::IApplication::RunMode& mode) override;

private:
    ::NyquistEffectsModule m_module;
};
}
