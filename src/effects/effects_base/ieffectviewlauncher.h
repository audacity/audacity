/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "global/types/ret.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectViewLauncher
{
public:
    virtual ~IEffectViewLauncher() = default;

    virtual muse::Ret showEffect(const EffectInstanceId& instanceId) const = 0;

    virtual void showRealtimeEffect(const RealtimeEffectStatePtr& state) const = 0;
    virtual void hideRealtimeEffect(const RealtimeEffectStatePtr& state) const = 0;
    virtual void toggleShowRealtimeEffect(const RealtimeEffectStatePtr& state) const = 0;
};

using IEffectViewLauncherPtr = std::shared_ptr<IEffectViewLauncher>;
}
