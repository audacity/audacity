/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/abstractviewlauncher.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectsprovider.h"

#include <set>

namespace au::effects {
class Lv2ViewLauncher final : public AbstractViewLauncher, public muse::async::Asyncable
{
    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    Lv2ViewLauncher(const muse::modularity::ContextPtr& ctx);

    muse::Ret showEffect(const EffectInstanceId& instanceId) const override;
    void showRealtimeEffect(const RealtimeEffectStatePtr& state) const override;
    bool vendorUiSupported(const EffectId& effectId) const override;
    void markVendorUiFailed(const EffectId& effectId) override;

private:
    std::set<EffectId> m_vendorUiFailed;
};
}
