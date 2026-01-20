/*
 * Audacity: A Digital Audio Editor
 */
#include "vstparameterextractorservice.h"

#include "framework/global/log.h"

// AU3 VST3 parameter extraction bridge (does not expose VST3 SDK types)
#include "au3-vst3/VST3ParameterExtraction.h"

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

    // "Full Range" (display) values - already converted from normalized in AU3
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

//! Convert parameter ID string to uint32_t (matches VST3 ParamID type)
//! Returns true on success, false on failure
bool parseParameterId(const String& parameterId, uint32_t& outParamId)
{
    bool ok = false;
    outParamId = parameterId.toUInt(&ok);
    if (!ok) {
        LOGW() << "VstParameterExtractorService: invalid parameter ID: " << parameterId;
    }
    return ok;
}
} // anonymous namespace

ParameterInfoList VstParameterExtractorService::extractParameters(EffectInstance* instance,
                                                                  EffectSettingsAccessPtr settingsAccess) const
{
    std::vector<VST3ParameterExtraction::ParamInfo> au3Params
        = VST3ParameterExtraction::extractParameters(instance, settingsAccess.get());

    // Populate cache for this instance
    auto& instanceCache = m_paramCache[instance];
    instanceCache.clear(); // Clear any existing cache for this instance

    ParameterInfoList result;
    result.reserve(au3Params.size());

    for (const auto& au3Param : au3Params) {
        // Cache the parameter info with its index
        instanceCache[au3Param.id] = au3Param;
        result.push_back(convertParamInfo(au3Param));
    }

    return result;
}

ParameterInfo VstParameterExtractorService::getParameter(EffectInstance* instance, const String& parameterId) const
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return {};
    }

    // Check cache first
    auto instanceIt = m_paramCache.find(instance);
    if (instanceIt != m_paramCache.end()) {
        auto paramIt = instanceIt->second.find(paramId);
        if (paramIt != instanceIt->second.end()) {
            // Found in cache - update current value and return
            VST3ParameterExtraction::ParamInfo& cachedParam = paramIt->second;

            // Update the current value from VST3 (value may have changed)
            const double normalizedValue = VST3ParameterExtraction::getParameterValue(instance, paramId);
            cachedParam.currentValue = cachedParam.toFullRange(normalizedValue);

            // Update the formatted string
            cachedParam.currentValueString = VST3ParameterExtraction::getParameterValueString(
                instance, paramId, normalizedValue);

            return convertParamInfo(cachedParam);
        }
    }

    // Not in cache - do full lookup
    VST3ParameterExtraction::ParamInfo au3Param = VST3ParameterExtraction::getParameter(instance, paramId);
    if (au3Param.id == 0 && au3Param.name.empty()) {
        return {}; // Not found
    }

    // Cache it for next time
    m_paramCache[instance][paramId] = au3Param;

    return convertParamInfo(au3Param);
}

double VstParameterExtractorService::getParameterValue(EffectInstance* instance, const String& parameterId) const
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return 0.0;
    }

    // Check cache for parameter info (to get min/max for conversion)
    auto instanceIt = m_paramCache.find(instance);
    if (instanceIt != m_paramCache.end()) {
        auto paramIt = instanceIt->second.find(paramId);
        if (paramIt != instanceIt->second.end()) {
            // Use cached parameter info to convert normalized to full range
            const double normalizedValue = VST3ParameterExtraction::getParameterValue(instance, paramId);
            return paramIt->second.toFullRange(normalizedValue);
        }
    }

    // Not in cache - just return normalized value (fallback)
    return VST3ParameterExtraction::getParameterValue(instance, paramId);
}

bool VstParameterExtractorService::setParameterValue(EffectInstance* instance,
                                                     const String& parameterId,
                                                     double fullRangeValue,
                                                     EffectSettingsAccessPtr settingsAccess)
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return false;
    }

    // Convert "Full Range" value to normalized [0,1] for VST3 API
    const double normalizedValue = VST3ParameterExtraction::fullRangeToNormalized(instance, paramId, fullRangeValue);

    const bool result = VST3ParameterExtraction::setParameterValue(instance, paramId, normalizedValue, settingsAccess.get());
    return result;
}

String VstParameterExtractorService::getParameterValueString(EffectInstance* instance,
                                                             const String& parameterId,
                                                             double value) const
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return {};
    }

    std::string result = VST3ParameterExtraction::getParameterValueString(instance, paramId, value);
    return String::fromStdString(result);
}

void VstParameterExtractorService::beginParameterGesture(EffectInstance* instance, const String& parameterId,
                                                         EffectSettingsAccessPtr settingsAccess)
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return;
    }

    // Store settings access for this gesture
    m_gestureSettings[instance] = settingsAccess;

    // Call VST3 beginEdit
    VST3ParameterExtraction::beginEdit(instance, paramId);
}

void VstParameterExtractorService::endParameterGesture(EffectInstance* instance, const String& parameterId)
{
    uint32_t paramId = 0;
    if (!parseParameterId(parameterId, paramId)) {
        return;
    }

    // Call VST3 endEdit
    VST3ParameterExtraction::endEdit(instance, paramId);

    // Get the stored settings access
    auto it = m_gestureSettings.find(instance);
    if (it != m_gestureSettings.end() && it->second) {
        // Now save the final state
        VST3ParameterExtraction::flushAndStoreSettings(instance, it->second.get());
        m_gestureSettings.erase(it);
    }
}

void VstParameterExtractorService::onInstanceDestroyed(EffectInstance* instance)
{
    clearCache(instance);
    m_gestureSettings.erase(instance);
}

void VstParameterExtractorService::beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess)
{
    VST3ParameterExtraction::beginParameterEditing(instance, settingsAccess.get());
}

void VstParameterExtractorService::endParameterEditing(EffectInstance* instance)
{
    VST3ParameterExtraction::endParameterEditing(instance);
}

void VstParameterExtractorService::clearCache(EffectInstance* instance)
{
    auto it = m_paramCache.find(instance);
    if (it != m_paramCache.end()) {
        m_paramCache.erase(it);
    }
}

void VstParameterExtractorService::clearAllCaches()
{
    m_paramCache.clear();
}
