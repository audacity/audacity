/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../effectstypes.h"

namespace au::effects {
class BuiltinEffects
{
public:
    
    BuiltinEffects() = default;

    static void init();

    EffectMetaList effectMetaList() const;
};
}
