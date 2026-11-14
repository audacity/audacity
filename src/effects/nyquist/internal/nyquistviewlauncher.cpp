/*
* Audacity: A Digital Audio Editor
*/
#include "nyquistviewlauncher.h"

using namespace au::effects;

muse::Ret NyquistViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::Nyquist);
}

void NyquistViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
