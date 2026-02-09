/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/vst/ivstinstancesregister.h"

#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/internal/abstractviewlauncher.h"

namespace au::effects {
class Vst3ViewLauncher final : public AbstractViewLauncher
{
    muse::Inject<muse::vst::IVstInstancesRegister> museInstancesRegister{ this };

public:
    Vst3ViewLauncher(const muse::modularity::ContextPtr& ctx)
        : AbstractViewLauncher(ctx) {}

    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;

private:
    void registerFxPlugin(const EffectInstanceId& instanceId) const;
};
}
