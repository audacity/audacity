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

private:
    void doInit(const muse::IApplication::RunMode& mode) override;

    ::NyquistEffectsModule m_module;
};
}
