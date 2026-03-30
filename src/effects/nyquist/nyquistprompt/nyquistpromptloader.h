/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "effects/builtin/ibuiltineffectsviewregister.h"
#include "effects/builtin/ibuiltineffectsrepository.h"

class WaveChannel;

namespace au::effects {
class NyquistPromptLoader
{
    muse::GlobalInject<IBuiltinEffectsRepository> builtinEffectsRepository;
    muse::GlobalInject<IBuiltinEffectsViewRegister> builtinEffectsViewRegister;

public:
    static void preInit();
    void init();
};
}
