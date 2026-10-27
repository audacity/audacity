/*
* Audacity: A Digital Audio Editor
*/
#include "nyquistviewlauncher.h"

using namespace au::effects;

muse::Ret NyquistViewLauncher::showEffect(const EffectInstanceId& instanceId, EffectViewMode mode) const
{
    return doShowEffect(instanceId, EffectFamily::Nyquist, mode);
}

void NyquistViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
