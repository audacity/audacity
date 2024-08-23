/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../effectstypes.h"

#include "modularity/ioc.h"
#include "../ieffectsviewregister.h"

namespace au::effects {
class BuiltinEffects
{
    inline static muse::Inject<IEffectsViewRegister> effectsViewRegister;
public:

    BuiltinEffects() = default;

    static void init();

    EffectMetaList effectMetaList() const;
};
}
