/*
* Audacity: A Digital Audio Editor
*/
#include "vst3viewlauncher.h"

#include "musevstplugininstance.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-vst3/VST3Instance.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

using namespace au::effects;

void Vst3ViewLauncher::registerFxPlugin(const EffectInstanceId& instanceId) const
{
    if (museInstancesRegister()->instanceById(instanceId)) {
        return;
    }

    std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(instanceId);
    IF_ASSERT_FAILED(instance) {
        return;
    }

    std::shared_ptr<VST3Instance> auVst3Instance = std::dynamic_pointer_cast<VST3Instance>(instance);
    IF_ASSERT_FAILED(auVst3Instance) {
        return;
    }

    const auto effectId = instancesRegister()->effectIdByInstanceId(instanceId);

    std::shared_ptr<MuseVstPluginInstance> museVstInstance = std::make_shared<MuseVstPluginInstance>(effectId, instanceId, auVst3Instance);
    museInstancesRegister()->registerFxPlugin(0, 0, museVstInstance);
}

muse::Ret Vst3ViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    auto museVstInstance = museInstancesRegister()->instanceById(instanceId);
    if (!museVstInstance) {
        registerFxPlugin(instanceId);
        museVstInstance = museInstancesRegister()->instanceById(instanceId);
    }
    IF_ASSERT_FAILED(museVstInstance) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    muse::UriQuery uri(muse::String(EFFECT_VIEWER_URI).toStdString());
    uri.addParam("instanceId", muse::Val(instanceId));
    uri.addParam("effectFamily", muse::Val(EffectFamily::VST3));

    muse::Ret ret = interactive()->openSync(uri).ret;

    return ret;
}

void Vst3ViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    const auto effectId = au3::wxToString(state->GetID());

    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << effectId;
        return;
    }

    instancesRegister()->regInstance(effectId, instance, state->GetAccess());
    registerFxPlugin(state->GetInstance()->id());
    doShowRealtimeEffect(state);
}
