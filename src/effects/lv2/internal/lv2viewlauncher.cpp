/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewlauncher.h"
#include "lv2uidiscovery.h"

#include "au3-components/EffectInterface.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-lv2/LV2EffectBase.h"

using namespace au::effects;

muse::Ret Lv2ViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::LV2);
}

void Lv2ViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}

bool Lv2ViewLauncher::canShowVendorUi(const EffectId& effectId) const
{
    const auto* const effect = dynamic_cast<const LV2EffectBase*>(effectsProvider()->effect(effectId));
    if (!effect) {
        return false;
    }
    return findHostableUi(effect->mPlug).isHostable();
}
