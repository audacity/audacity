/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"

#include "../ieffectviewlaunchregister.h"

#include "../ieffectviewcontroller.h"
#include "../ieffectsprovider.h"
#include "../irealtimeeffectservice.h"

class EffectBase;
class EffectSettingsAccess;
class TrackList;

namespace au::effects {
class EffectViewController : public IEffectViewController, public muse::Contextable
{
    muse::GlobalInject<IEffectsProvider> effectsProvider;

    muse::ContextInject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<IEffectViewLaunchRegister> viewLaunchRegister{ this };

public:
    EffectViewController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;

    void showEffect(const RealtimeEffectStatePtr& state) const override;
    void hideEffect(const RealtimeEffectStatePtr& state) const override;
};
}
