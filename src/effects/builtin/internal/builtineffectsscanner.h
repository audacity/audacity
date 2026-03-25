/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ibuiltineffectsrepository.h"

#include "framework/audioplugins/iaudiopluginsscanner.h"
#include "framework/global/modularity/ioc.h"

namespace au::effects {
class BuiltinEffectsScanner : public muse::audioplugins::IAudioPluginsScanner
{
    muse::GlobalInject<IBuiltinEffectsRepository> builtinEffectsRepository;

public:
    muse::io::paths_t scanPlugins() const override;
};
}
