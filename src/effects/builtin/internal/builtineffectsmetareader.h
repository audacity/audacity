/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginmetareader.h"

#include "au3-effects/LoadEffects.h"

namespace au::effects {
class BuiltinEffectsMetaReader : public Au3AudioPluginMetaReader
{
public:
    BuiltinEffectsMetaReader();

    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;

private:
    ::BuiltinEffectsModule m_builtinEffectsModule;
};
}
