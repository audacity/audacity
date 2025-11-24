/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewlauncher.h"

#include "au3-components/EffectInterface.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

using namespace au::effects;

muse::Ret Lv2ViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::LV2);
}

void Lv2ViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
