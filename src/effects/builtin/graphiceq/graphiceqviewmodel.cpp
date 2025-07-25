/*
* Audacity: A Digital Audio Editor
*/
#include "graphiceqviewmodel.h"

#include "graphiceq.h"

#include "log.h"

namespace au::effects {
GraphicEq* GraphicEqViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<GraphicEq*>(e);
}

void GraphicEqViewModel::doReload()
{
}
}
