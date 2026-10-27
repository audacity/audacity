/*
* Audacity: A Digital Audio Editor
*/
#include "builtinviewlauncher.h"

using namespace au::effects;

muse::Ret BuiltinViewLauncher::showEffect(const EffectInstanceId& instanceId, EffectViewMode mode) const
{
    return doShowEffect(instanceId, EffectFamily::Builtin, mode);
}

void BuiltinViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
