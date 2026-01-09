/*
* Audacity: A Digital Audio Editor
*/
#include "effectparametersprovider.h"

#include "framework/global/log.h"

#include "../effectstypes.h"

using namespace au::effects;
using namespace muse;

ParameterInfoList EffectParametersProvider::parameters(EffectInstanceId instanceId) const
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return {};
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    EffectFamily family = getEffectFamily(effectId);

    const IParameterExtractorService* extractor = parameterExtractorRegistry()
                                                  ? parameterExtractorRegistry()->extractorForFamily(family)
                                                  : nullptr;
    if (!extractor) {
        LOGW() << "No parameter extractor registered for effect family: " << static_cast<int>(family);
        return {};
    }

    const EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId);
    return extractor->extractParameters(instance, settingsAccess);
}

ParameterInfo EffectParametersProvider::parameter(EffectInstanceId instanceId, const String& parameterId) const
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return {};
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    EffectFamily family = getEffectFamily(effectId);

    const IParameterExtractorService* extractor = parameterExtractorRegistry()
                                                  ? parameterExtractorRegistry()->extractorForFamily(family)
                                                  : nullptr;
    if (!extractor) {
        LOGW() << "No parameter extractor registered for effect family: " << static_cast<int>(family);
        return {};
    }

    return extractor->getParameter(instance, parameterId);
}

double EffectParametersProvider::parameterValue(EffectInstanceId instanceId, const String& parameterId) const
{
    const ParameterInfo param = parameter(instanceId, parameterId);
    if (!param.isValid()) {
        return 0.0;
    }
    return param.currentValue;
}

bool EffectParametersProvider::setParameterValue(EffectInstanceId instanceId, const String& parameterId, double normalizedValue)
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return false;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    EffectFamily family = getEffectFamily(effectId);

    IParameterExtractorService* extractor = parameterExtractorRegistry()
                                            ? parameterExtractorRegistry()->extractorForFamily(family)
                                            : nullptr;
    if (!extractor) {
        LOGW() << "No parameter extractor registered for effect family: " << static_cast<int>(family);
        return false;
    }

    const EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId);
    const bool success = extractor->setParameterValue(instance, parameterId, normalizedValue, settingsAccess);

    if (success) {
        // Get the updated parameter info to send plain value and formatted string
        const ParameterInfo param = parameter(instanceId, parameterId);

        ParameterChangedData data;
        data.instanceId = instanceId;
        data.parameterId = parameterId;
        data.newFullRangeValue = param.currentValue;
        data.newValueString = param.currentValueString;

        m_parameterChanged.send(data);
    }

    return success;
}

String EffectParametersProvider::parameterValueString(EffectInstanceId instanceId, const String& parameterId, double value) const
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return String();
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    const EffectFamily family = getEffectFamily(effectId);

    const IParameterExtractorService* extractor = parameterExtractorRegistry()
                                                  ? parameterExtractorRegistry()->extractorForFamily(family)
                                                  : nullptr;
    if (extractor) {
        return extractor->getParameterValueString(instance, parameterId, value);
    }

    // Fallback: simple default formatting
    const ParameterInfo param = parameter(instanceId, parameterId);
    if (!param.isValid()) {
        return String();
    }

    if (!param.units.empty()) {
        return String::number(value) + u" " + param.units;
    }
    return String::number(value);
}

bool EffectParametersProvider::supportsParameterExtraction(const EffectId& effectId) const
{
    const EffectFamily family = getEffectFamily(effectId);
    return parameterExtractorRegistry() && parameterExtractorRegistry()->hasExtractorForFamily(family);
}

muse::async::Channel<ParameterChangedData> EffectParametersProvider::parameterChanged() const
{
    return m_parameterChanged;
}

EffectFamily EffectParametersProvider::getEffectFamily(const EffectId& effectId) const
{
    // Get the effect metadata which contains the family information
    EffectMeta effectMeta = effectsProvider()->meta(effectId);
    if (effectMeta.isValid()) {
        return effectMeta.family;
    }

    LOGW() << "Could not determine effect family for: " << effectId;
    return EffectFamily::Unknown;
}
