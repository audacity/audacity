/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginviewlauncher.h"

namespace au::effects {
muse::Ret AudacityPluginViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::AudacityPlugin);
}

void AudacityPluginViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}
} // namespace au::effects
