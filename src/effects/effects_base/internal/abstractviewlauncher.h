#pragma once

#include "../ieffectviewlauncher.h"

#include "effects/effects_base/ieffectinstancesregister.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"

namespace au::effects {
class AbstractViewLauncher : public IEffectViewLauncher
{
public:
    static int initializationInstanceId();

protected:
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<IEffectInstancesRegister> instancesRegister;

    muse::Ret doShowEffect(int instanceId, EffectFamily) const;
    void doShowRealtimeEffect(const RealtimeEffectStatePtr& state) const;

private:
    void hideRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
}
