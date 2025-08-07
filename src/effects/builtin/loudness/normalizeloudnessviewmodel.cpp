/*
 * Audacity: A Digital Audio Editor
 */
#include "normalizeloudnessviewmodel.h"

#include "normalizeloudnesseffect.h"

#include "log.h"

namespace au::effects {
NormalizeLoudnessEffect* NormalizeLoudnessViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<NormalizeLoudnessEffect*>(e);
}

void NormalizeLoudnessViewModel::doReload()
{
}
}
