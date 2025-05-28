/*
* Audacity: A Digital Audio Editor
*/
#include "builtinviewlauncher.h"

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

muse::Ret BuiltinViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    muse::UriQuery uri(muse::String(EFFECT_VIEWER_URI).toStdString());
    uri.addParam("instanceId", muse::Val(instanceId));
    uri.addParam("effectFamily", muse::Val(EffectFamily::Builtin));
    return interactive()->openSync(uri).ret;
}

void BuiltinViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
