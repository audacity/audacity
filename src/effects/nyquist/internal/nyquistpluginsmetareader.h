/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginmetareader.h"

#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistPluginsMetaReader : public Au3AudioPluginMetaReader
{
public:
    NyquistPluginsMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;

private:
    ::NyquistEffectsModule m_module;
};
}
