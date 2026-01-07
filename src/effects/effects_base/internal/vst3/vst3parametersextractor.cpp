/*
* Audacity: A Digital Audio Editor
*/
#include "vst3parametersextractor.h"

#include "framework/global/log.h"
#include "au3wrap/internal/wxtypes_convert.h"

// AU3 VST3 parameter extraction bridge (does not expose VST3 SDK types)
#include "au3-vst3/VST3ParameterExtraction.h"

#include "au3-components/EffectInterface.h"  // For EffectSettingsAccess

using namespace au::effects;
using namespace muse;

namespace {
//! Convert AU3 ParamType to AU4 ParameterType
ParameterType convertParamType(VST3ParameterExtraction::ParamType type)
{
    switch (type) {
    case VST3ParameterExtraction::ParamType::Toggle:
        return ParameterType::Toggle;
    case VST3ParameterExtraction::ParamType::Dropdown:
        return ParameterType::Dropdown;
    case VST3ParameterExtraction::ParamType::Slider:
        return ParameterType::Slider;
    case VST3ParameterExtraction::ParamType::Numeric:
        return ParameterType::Numeric;
    case VST3ParameterExtraction::ParamType::ReadOnly:
        return ParameterType::ReadOnly;
    case VST3ParameterExtraction::ParamType::Unknown:
    default:
        return ParameterType::Unknown;
    }
}

//! Convert AU3 ParamInfo to AU4 ParameterInfo
ParameterInfo convertParamInfo(const VST3ParameterExtraction::ParamInfo& au3Info)
{
    ParameterInfo au4Info;

    au4Info.id = String::number(static_cast<size_t>(au3Info.id));
    au4Info.name = String::fromStdString(au3Info.name);
    au4Info.description = String::fromStdString(au3Info.description);
    au4Info.units = String::fromStdString(au3Info.units);
    au4Info.group = String::fromStdString(au3Info.group);

    au4Info.type = convertParamType(au3Info.type);

    // Plain (display) values - already converted from normalized in AU3
    au4Info.minValue = au3Info.minValue;
    au4Info.maxValue = au3Info.maxValue;
    au4Info.defaultValue = au3Info.defaultValue;
    au4Info.currentValue = au3Info.currentValue;

    au4Info.currentValueString = String::fromStdString(au3Info.currentValueString);

    au4Info.stepCount = au3Info.stepCount;
    au4Info.stepSize = au3Info.stepSize;

    // Convert enum values
    au4Info.enumValues.reserve(au3Info.enumValues.size());
    for (const auto& enumVal : au3Info.enumValues) {
        au4Info.enumValues.push_back(String::fromStdString(enumVal));
    }
    au4Info.enumIndices = au3Info.enumIndices;

    au4Info.isReadOnly = au3Info.isReadOnly;
    au4Info.isHidden = au3Info.isHidden;
    au4Info.isLogarithmic = au3Info.isLogarithmic;
    au4Info.isInteger = au3Info.isInteger;
    au4Info.canAutomate = au3Info.canAutomate;

    return au4Info;
}
} // anonymous namespace

ParameterInfoList VST3ParametersExtractor::extractParameters(EffectInstanceEx* instance,
                                                             const std::shared_ptr<EffectSettingsAccess>& settingsAccess)
{
    // Delegate to AU3 bridge function (does not expose VST3 SDK types)
    std::vector<VST3ParameterExtraction::ParamInfo> au3Params
        =VST3ParameterExtraction::extractParameters(instance, settingsAccess.get());

    // Convert AU3 ParamInfo to AU4 ParameterInfo
    ParameterInfoList result;
    result.reserve(au3Params.size());

    for (const auto& au3Param : au3Params) {
        result.push_back(convertParamInfo(au3Param));
    }

    return result;
}

double VST3ParametersExtractor::getParameterValue(EffectInstanceEx* instance, const String& parameterId)
{
    // Convert parameter ID string to uint32_t (matches VST3 ParamID type)
    bool ok = false;
    uint32_t paramId = parameterId.toUInt(&ok);
    if (!ok) {
        LOGW() << "VST3ParametersExtractor: invalid parameter ID: " << parameterId;
        return 0.0;
    }

    // Delegate to AU3 bridge function
    return VST3ParameterExtraction::getParameterValue(instance, paramId);
}

bool VST3ParametersExtractor::setParameterValue(EffectInstanceEx* instance, const String& parameterId, double value,
                                                const std::shared_ptr<EffectSettingsAccess>& settingsAccess)
{
    // Convert parameter ID string to uint32_t (matches VST3 ParamID type)
    bool ok = false;
    uint32_t paramId = parameterId.toUInt(&ok);
    if (!ok) {
        LOGW() << "VST3ParametersExtractor: invalid parameter ID: " << parameterId;
        return false;
    }

    // Delegate to AU3 bridge function
    return VST3ParameterExtraction::setParameterValue(instance, paramId, value, settingsAccess.get());
}

String VST3ParametersExtractor::getParameterValueString(EffectInstanceEx* instance,
                                                        const String& parameterId,
                                                        double value)
{
    // Convert parameter ID string to uint32_t (matches VST3 ParamID type)
    bool ok = false;
    uint32_t paramId = parameterId.toUInt(&ok);
    if (!ok) {
        return String();
    }

    // Delegate to AU3 bridge function
    std::string result = VST3ParameterExtraction::getParameterValueString(instance, paramId, value);
    return String::fromStdString(result);
}
