/*
* Audacity: A Digital Audio Editor
*/
#include "clickremovalviewmodel.h"
#include "clickremovaleffect.h"

#include "log.h"

namespace au::effects {
ClickRemovalEffect* ClickRemovalViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<ClickRemovalEffect*>(e);
}

void ClickRemovalViewModel::doReload()
{
}
}
