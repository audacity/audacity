/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "effects/effects_base/ieffectinstancesregister.h"

namespace au::effects {
class Lv2ViewLauncher final : public AbstractViewLauncher
{
public:
    Lv2ViewLauncher(const muse::modularity::ContextPtr& ctx)
        : AbstractViewLauncher(ctx) {}
    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
}
