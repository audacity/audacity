/*
* Audacity: A Digital Audio Editor
*/
#include "audiounitviewlauncher.h"

#include "log.h"

muse::Ret au::effects::AudioUnitViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    muse::UriQuery uri(muse::String(EFFECT_VIEWER_URI).toStdString());
    uri.addParam("instanceId", muse::Val(instanceId));
    uri.addParam("effectFamily", muse::Val(EffectFamily::AudioUnit));
    return interactive()->openSync(uri).ret;
}

void au::effects::AudioUnitViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
