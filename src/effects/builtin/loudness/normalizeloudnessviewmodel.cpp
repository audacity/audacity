/*
 * Audacity: A Digital Audio Editor
 */
#include "normalizeloudnessviewmodel.h"

#include "libraries/lib-builtin-effects/LoudnessBase.h"

#include "log.h"

namespace au::effects {
LoudnessBase* NormalizeLoudnessViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<LoudnessBase*>(e);
}

void NormalizeLoudnessViewModel::doReload()
{
}
}
