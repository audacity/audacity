/*
 * Audacity: A Digital Audio Editor
 */
#include "noisereductionviewmodel.h"

#include "libraries/lib-builtin-effects/NoiseReductionBase.h"

#include "log.h"

using namespace au::effects;

NoiseReductionBase* NoiseReductionViewModel::effect() const
{
    EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* e = effectsProvider()->effect(effectId);
    return dynamic_cast<NoiseReductionBase*>(e);
}

void NoiseReductionViewModel::doReload()
{
}

bool NoiseReductionViewModel::isApplyAllowed() const
{
    return m_isApplyAllowed;
}

void NoiseReductionViewModel::setIsApplyAllowed(bool isApplyAllowed)
{
    if (m_isApplyAllowed == isApplyAllowed) {
        return;
    }
    m_isApplyAllowed = isApplyAllowed;
    emit isApplyAllowedChanged();
}
