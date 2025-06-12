/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

#include "global/modularity/ioc.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "global/iinteractive.h"

namespace au::effects {
class Lv2ViewLauncher final : public AbstractViewLauncher
{
public:
    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
}
