#pragma once

#include "../ieffectviewlauncher.h"

#include "effects/effects_base/ieffectinstancesregister.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"

namespace au::effects {
class AbstractViewLauncher : public IEffectViewLauncher, public muse::Injectable
{
protected:
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<IEffectInstancesRegister> instancesRegister{ this };

    muse::Ret doShowEffect(int instanceId, EffectFamily) const;
    void doShowRealtimeEffect(const RealtimeEffectStatePtr& state) const;

private:
    void hideRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
}
