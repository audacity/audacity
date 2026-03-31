/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/ibuiltineffectsviewregister.h"
#include "framework/global/modularity/ioc.h"

namespace au::effects {
class BuiltinCollectionLoader : public muse::Contextable
{
    muse::GlobalInject<IBuiltinEffectsViewRegister> builtinEffectsViewRegister;

public:
    BuiltinCollectionLoader(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    static void preInit();
    void init();
};
}
