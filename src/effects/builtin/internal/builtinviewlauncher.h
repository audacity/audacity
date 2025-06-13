/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

namespace au::effects {
class BuiltinViewLauncher final : public AbstractViewLauncher
{
public:
    BuiltinViewLauncher() = default;

    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
}
