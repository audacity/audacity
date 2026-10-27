/*
* Audacity: A Digital Audio Editor
*/
#include "audiounitviewlauncher.h"

muse::Ret au::effects::AudioUnitViewLauncher::showEffect(const EffectInstanceId& instanceId, EffectViewMode mode) const
{
    return doShowEffect(instanceId, EffectFamily::AudioUnit, mode);
}

void au::effects::AudioUnitViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
