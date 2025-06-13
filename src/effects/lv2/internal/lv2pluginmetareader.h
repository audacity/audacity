/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/abstractaudiopluginmetareader.h"
#include "libraries/lib-lv2/LoadLV2.h"

namespace au::effects {
class Lv2PluginMetaReader final : public AbstractAudioPluginMetaReader
{
public:
    Lv2PluginMetaReader();
    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;

private:
    void doInit(const muse::IApplication::RunMode& mode) override;
    LV2EffectsModule m_module;
};
}
