/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "effects/builtin/ibuiltineffectsviewregister.h"
#include "effects/builtin/ibuiltineffectsrepository.h"
#include "modularity/ioc.h"

namespace au::effects {
class BuiltinEffectsLoader : public muse::Contextable
{
    muse::GlobalInject<IBuiltinEffectsRepository> builtinEffectsRepository;
    muse::GlobalInject<IBuiltinEffectsViewRegister> builtinEffectsViewRegister;

public:
    BuiltinEffectsLoader(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    static void preInit();
    void init();
};
}
