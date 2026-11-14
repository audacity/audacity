/*
* Audacity: A Digital Audio Editor
*/
#include "audiounitviewlauncher.h"

muse::Ret au::effects::AudioUnitViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::AudioUnit);
}

void au::effects::AudioUnitViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
