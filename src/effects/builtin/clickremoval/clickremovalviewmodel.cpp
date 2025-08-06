/*
* Audacity: A Digital Audio Editor
*/
#include "clickremovalviewmodel.h"

#include "libraries/lib-builtin-effects/ClickRemovalBase.h"

#include "log.h"

namespace au::effects {
ClickRemovalBase* ClickRemovalViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<ClickRemovalBase*>(e);
}

void ClickRemovalViewModel::doReload()
{
}
}
