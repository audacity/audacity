/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewlauncher.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

using namespace au::effects;

muse::Ret Lv2ViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    muse::UriQuery uri(muse::String(EFFECT_VIEWER_URI).toStdString());
    uri.addParam("instanceId", muse::Val(instanceId));
    uri.addParam("effectFamily", muse::Val(EffectFamily::LV2));
    return interactive()->openSync(uri).ret;
}

void Lv2ViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
