/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsviewregister.h"

#include "../ibuiltineffectsrepository.h"

namespace au::effects {
class BuiltinEffectsRepository : public IBuiltinEffectsRepository
{
    muse::Inject<IEffectsViewRegister> effectsViewRegister;

public:
    BuiltinEffectsRepository() = default;

    void init();

    EffectMetaList effectMetaList() const override;
};
}
