/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/ibuiltineffectsviewregister.h"
#include "effects/effects_base/ieffectsprovider.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

class WaveChannel;

namespace au::effects {
class NyquistPromptLoader : public muse::async::Asyncable
{
    muse::GlobalInject<IBuiltinEffectsViewRegister> builtinEffectsViewRegister;
    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    static void preInit();
    void init();
};
}
