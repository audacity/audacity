/*
 * Audacity: A Digital Audio Editor
 */
#include "extensionviewlauncher.h"

namespace au::effects::extensions {
muse::Ret ExtensionViewLauncher::showEffect(const EffectInstanceId& instanceId, EffectViewMode mode) const
{
    return doShowEffect(instanceId, EffectFamily::Extension, mode);
}

void ExtensionViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr&) const
{
}
} // namespace au::effects::extensions
