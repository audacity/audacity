/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "effects/effects_base/ieffectviewlauncher.h"

#include "global/modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "global/iinteractive.h"

// from muse
#include "vst/ivstinstancesregister.h"

namespace au::effects {
class Vst3ViewLauncher : public IEffectViewLauncher
{
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<muse::vst::IVstInstancesRegister> museInstancesRegister;
    muse::Inject<muse::IInteractive> interactive;

public:
    Vst3ViewLauncher() = default;

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;
};
}
