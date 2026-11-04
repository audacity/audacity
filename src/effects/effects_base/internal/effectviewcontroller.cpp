/*
* Audacity: A Digital Audio Editor
*/
#include "effectviewcontroller.h"
#include "effectsutils.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-effects/Effect.h"
#include "au3-components/EffectInterface.h"
#include "au3-effects/EffectManager.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

#include "framework/global/log.h"

namespace au::effects {
namespace {
IEffectViewLauncherPtr getLauncher(const EffectId& effectId, const IEffectsProvider& effectsProvider,
                                   const IEffectViewLaunchRegister& launchRegister)
{
    const auto family = effectsProvider.meta(effectId).family;
    const auto launcher = launchRegister.launcher(family);
    IF_ASSERT_FAILED(launcher) {
        LOGE() << "not found launcher for family:" << utils::effectFamilyToString(family);
        return {};
    }
    return launcher;
}

void callOnLauncher(const RealtimeEffectStatePtr& state, const IEffectsProvider& effectsProvider,
                    const IEffectViewLaunchRegister& launchRegister,
                    std::function<void(const IEffectViewLauncher&, const RealtimeEffectStatePtr&)> func)
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    if (const auto launcher = getLauncher(au::au3::wxToString(state->GetID()), effectsProvider, launchRegister)) {
        func(*launcher, state);
    }
}
}

muse::Ret EffectViewController::showEffect(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    LOGD() << "try open effect: " << effectId << ", instanceId: " << instanceId;

    if (!effectsProvider()->loadEffect(effectId)) {
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    const auto launcher = getLauncher(effectId, *effectsProvider(), *viewLaunchRegister());
    if (!launcher) {
        return muse::make_ret(muse::Ret::Code::NotSupported);
    }

    const muse::Ret ret = launcher->showEffect(instanceId);

    LOGD() << "open ret: " << ret.toString();
    return ret;
}

void EffectViewController::showEffect(const RealtimeEffectStatePtr& state) const
{
    if (!realtimeEffectService()->isAvailable(state)) {
        return;
    }

    callOnLauncher(state, *effectsProvider(), *viewLaunchRegister(),
                   [](const IEffectViewLauncher& launcher, const RealtimeEffectStatePtr& state) {
        launcher.showRealtimeEffect(state);
    });
}

void EffectViewController::hideEffect(const RealtimeEffectStatePtr& state) const
{
    if (!realtimeEffectService()->isAvailable(state)) {
        return;
    }

    callOnLauncher(state, *effectsProvider(), *viewLaunchRegister(),
                   [](const IEffectViewLauncher& launcher, const RealtimeEffectStatePtr& state) {
        launcher.hideRealtimeEffect(state);
    });
}
}
