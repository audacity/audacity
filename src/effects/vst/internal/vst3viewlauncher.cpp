/*
* Audacity: A Digital Audio Editor
*/
#include "vst3viewlauncher.h"

#include "musevstplugininstance.h"

#include "au3-components/EffectInterface.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-vst3/VST3Instance.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/log.h"

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

    return doShowEffect(instanceId, EffectFamily::VST3);
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

bool Vst3ViewLauncher::vendorUiSupported(const EffectId& effectId) const
{
    // Unlike LV2 there is no cheap way to know up front whether a VST3 ships an
    // editor: we find out when VstView asks for it (createView returns null) and are
    // told via markVendorUiFailed, after which the generic UI is used for that effect.
    return m_vendorUiFailed.count(effectId) == 0;
}

void Vst3ViewLauncher::markVendorUiFailed(const EffectId& effectId)
{
    m_vendorUiFailed.insert(effectId);
}
