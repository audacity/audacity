/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "../ieffectparametersprovider.h"
#include "../ieffectinstancesregister.h"
#include "../ieffectsprovider.h"

namespace au::effects {
class EffectParametersProvider : public IEffectParametersProvider, public muse::async::Asyncable
{
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    EffectParametersProvider() = default;

    // IEffectParametersProvider interface
    ParameterInfoList parameters(EffectInstanceId instanceId) const override;
    ParameterInfo parameter(EffectInstanceId instanceId, const muse::String& parameterId) const override;

    double parameterValue(EffectInstanceId instanceId, const muse::String& parameterId) const override;
    bool setParameterValue(EffectInstanceId instanceId, const muse::String& parameterId, double value) override;

    muse::String parameterValueString(EffectInstanceId instanceId, const muse::String& parameterId, double value) const override;

    bool supportsParameterExtraction(const EffectId& effectId) const override;

    muse::async::Channel<ParameterChangedData> parameterChanged() const override;

private:
    // Extract parameters from VST3 plugin
    ParameterInfoList extractVST3Parameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess) const;

    // Extract parameters from LV2 plugin
    ParameterInfoList extractLV2Parameters(EffectInstance* instance) const;

    // Extract parameters from Audio Unit plugin
    ParameterInfoList extractAudioUnitParameters(EffectInstance* instance) const;

    // Helper to determine effect family
    EffectFamily getEffectFamily(const EffectId& effectId) const;

    mutable muse::async::Channel<ParameterChangedData> m_parameterChanged;
};
}
