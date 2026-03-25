/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ibuiltineffectsrepository.h"

#include "framework/audioplugins/iaudiopluginmetareader.h"
#include "framework/global/modularity/ioc.h"

namespace au::effects {
class BuiltinEffectsMetaReader : public muse::audioplugins::IAudioPluginMetaReader
{
    muse::GlobalInject<IBuiltinEffectsRepository> builtinEffectsRepository;

public:

    muse::audio::AudioResourceType metaType() const override;
    bool canReadMeta(const muse::io::path_t& pluginPath) const override;
    muse::RetVal<muse::audio::AudioResourceMetaList> readMeta(const muse::io::path_t& pluginPath) const override;
};
}
