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

    virtual bool vendorUiSupported(const EffectId&) const { return true; }

    virtual void markVendorUiFailed(const EffectId&) {}
};

using IEffectViewLauncherPtr = std::shared_ptr<IEffectViewLauncher>;
}
