/*
* Audacity: A Digital Audio Editor
*/
#include "effectparametersprovider.h"

#include "framework/global/log.h"

#include "../effectstypes.h"

#ifdef USE_VST3
#include "vst3/vst3parametersextractor.h"
#endif

// Note: VST3 parameter extraction is implemented in vst3/vst3parametersextractor.cpp
// to avoid including VST3 SDK headers in this file

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

    switch (family) {
    case EffectFamily::VST3:
    {
        // Get settings access to apply stored settings before extraction
        EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId);
        return extractVST3Parameters(instance, settingsAccess);
    }
    case EffectFamily::LV2:
        return extractLV2Parameters(instance);
    case EffectFamily::AudioUnit:
        return extractAudioUnitParameters(instance);
    case EffectFamily::Builtin:
    case EffectFamily::Unknown:
    default:
        LOGW() << "Parameter extraction not supported for effect family: " << static_cast<int>(family);
        return {};
    }
}

ParameterInfo EffectParametersProvider::parameter(EffectInstanceId instanceId, const String& parameterId) const
{
    ParameterInfoList params = parameters(instanceId);
    for (const auto& param : params) {
        if (param.id == parameterId) {
            return param;
        }
    }

    LOGW() << "Parameter not found: " << parameterId;
    return ParameterInfo(); // Return invalid parameter
}

double EffectParametersProvider::parameterValue(EffectInstanceId instanceId, const String& parameterId) const
{
    ParameterInfo param = parameter(instanceId, parameterId);
    if (!param.isValid()) {
        return 0.0;
    }
    return param.currentValue;
}

bool EffectParametersProvider::setParameterValue(EffectInstanceId instanceId, const String& parameterId, double value)
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return false;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    EffectFamily family = getEffectFamily(effectId);

    switch (family) {
    case EffectFamily::VST3:
#ifdef USE_VST3
    {
        // Get the settings access to properly persist the parameter change
        EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(instanceId);

        if (VST3ParametersExtractor::setParameterValue(instance, parameterId, value, settingsAccess)) {
            m_parameterValuesChanged.notify();
            return true;
        }
        return false;
    }
#else
        LOGW() << "VST3 support not enabled in this build";
        return false;
#endif
    case EffectFamily::LV2:
        // TODO: Implement LV2 parameter value setting
        LOGW() << "LV2 parameter value setting not yet implemented";
        return false;
    case EffectFamily::AudioUnit:
        // TODO: Implement Audio Unit parameter value setting
        LOGW() << "Audio Unit parameter value setting not yet implemented";
        return false;
    case EffectFamily::Builtin:
    case EffectFamily::Unknown:
    default:
        LOGW() << "Parameter value setting not supported for effect family: " << static_cast<int>(family);
        return false;
    }
}

String EffectParametersProvider::parameterValueString(EffectInstanceId instanceId, const String& parameterId, double value) const
{
    EffectInstance* instance = instancesRegister()->instanceById(instanceId).get();
    if (!instance) {
        LOGE() << "Effect instance not found: " << instanceId;
        return String();
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    EffectFamily family = getEffectFamily(effectId);

    switch (family) {
    case EffectFamily::VST3:
#ifdef USE_VST3
        return VST3ParametersExtractor::getParameterValueString(instance, parameterId, value);
#else
        break;
#endif
    case EffectFamily::LV2:
        // TODO: Implement LV2 parameter value string formatting
        LOGW() << "LV2 parameter value string formatting not yet implemented";
        break;
    case EffectFamily::AudioUnit:
        // TODO: Implement Audio Unit parameter value string formatting
        LOGW() << "Audio Unit parameter value string formatting not yet implemented";
        break;
    case EffectFamily::Builtin:
    case EffectFamily::Unknown:
    default:
        break;
    }

    // Fallback: simple default formatting
    ParameterInfo param = parameter(instanceId, parameterId);
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
    EffectFamily family = getEffectFamily(effectId);
    return family == EffectFamily::VST3
           || family == EffectFamily::LV2
           || family == EffectFamily::AudioUnit;
}

muse::async::Notification EffectParametersProvider::parameterValuesChanged() const
{
    return m_parameterValuesChanged;
}

ParameterInfoList EffectParametersProvider::extractVST3Parameters(EffectInstance* instance,
                                                                  EffectSettingsAccessPtr settingsAccess) const
{
#ifdef USE_VST3
    return VST3ParametersExtractor::extractParameters(instance, settingsAccess);
#else
    LOGW() << "VST3 support not enabled in this build";
    return {};
#endif
}

ParameterInfoList EffectParametersProvider::extractLV2Parameters(EffectInstance* instance) const
{
    // TODO: Implement LV2 parameter extraction
    // This will use the LV2Wrapper to query control ports
    LOGW() << "LV2 parameter extraction not yet implemented";
    return {};
}

ParameterInfoList EffectParametersProvider::extractAudioUnitParameters(EffectInstance* instance) const
{
#ifdef USE_AUDIO_UNITS
    // TODO: Implement Audio Unit parameter extraction
    // This will use the AudioUnitWrapper to query parameter list
    LOGW() << "Audio Unit parameter extraction not yet implemented";
    return {};
#else
    return {};
#endif
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
