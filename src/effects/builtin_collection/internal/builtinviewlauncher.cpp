/*
* Audacity: A Digital Audio Editor
*/
#include "builtinviewlauncher.h"

using namespace au::effects;

muse::Ret BuiltinViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::Builtin);
}

void BuiltinViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
