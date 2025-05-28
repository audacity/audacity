/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

#include "global/modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "global/iinteractive.h"

// from muse
#include "vst/ivstinstancesregister.h"

namespace au::effects {
class Vst3ViewLauncher final : public AbstractViewLauncher
{
    muse::Inject<muse::vst::IVstInstancesRegister> museInstancesRegister;

public:
    Vst3ViewLauncher() = default;

    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;

private:
    void registerFxPlugin(const EffectInstanceId& instanceId) const;
};
}
