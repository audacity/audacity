/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../effectstypes.h"

namespace au::effects {
class BuildInEffects
{
public:

    BuildInEffects() = default;

    static void init();

    EffectMetaList effectMetaList() const;
};
}
