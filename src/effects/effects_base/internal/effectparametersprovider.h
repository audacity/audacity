/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "../ieffectparametersprovider.h"
#include "../ieffectinstancesregister.h"
#include "../ieffectsprovider.h"
#include "../iparameterextractorregistry.h"

namespace au::effects {
class EffectParametersProvider : public IEffectParametersProvider, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IParameterExtractorRegistry> parameterExtractorRegistry;
    muse::GlobalInject<IEffectsProvider> effectsProvider;

    muse::ContextInject<IEffectInstancesRegister> instancesRegister{ this };

public:
    EffectParametersProvider(const muse::modularity::ContextPtr& ctx);

    // IEffectParametersProvider interface
    ParameterInfoList parameters(EffectInstanceId instanceId) const override;
    ParameterInfo parameter(EffectInstanceId instanceId, const muse::String& parameterId) const override;

    double parameterValue(EffectInstanceId instanceId, const muse::String& parameterId) const override;
    bool setParameterValue(EffectInstanceId instanceId, const muse::String& parameterId, double value) override;
    bool setParameterStringValue(EffectInstanceId instanceId, const muse::String& parameterId, const muse::String& stringValue) override;

    muse::String parameterValueString(EffectInstanceId instanceId, const muse::String& parameterId, double value) const override;

    bool supportsParameterExtraction(const EffectId& effectId) const override;

    void beginParameterGesture(EffectInstanceId instanceId, const muse::String& parameterId) override;
    void endParameterGesture(EffectInstanceId instanceId, const muse::String& parameterId) override;

    muse::async::Channel<ParameterChangedData> parameterChanged() const override;

private:
    // Helper to determine effect family
    EffectFamily getEffectFamily(const EffectId& effectId) const;

    mutable muse::async::Channel<ParameterChangedData> m_parameterChanged;
};
}
