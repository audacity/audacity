/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

namespace au::effects {
class AudacityPluginViewLauncher final : public AbstractViewLauncher
{
public:
    explicit AudacityPluginViewLauncher(const muse::modularity::ContextPtr& context)
        : AbstractViewLauncher(context) {}

    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
};
} // namespace au::effects
