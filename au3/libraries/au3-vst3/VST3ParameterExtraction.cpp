/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3ParameterExtraction.cpp

  @brief Bridge for extracting VST3 parameters without exposing VST3 SDK types

**********************************************************************/

#include "VST3ParameterExtraction.h"

#include "VST3Instance.h"
#include "VST3Utils.h"
#include "VST3Wrapper.h"

#include "au3-components/EffectInterface.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstunits.h>
#include <pluginterfaces/vst/ivstcomponent.h>

#include <wx/log.h>

#include <algorithm>
#include <stdexcept>
#include <sstream>

namespace VST3ParameterExtraction {
namespace {
//! Convert VST3 parameter flags to ParamType
ParamType getParameterType(const Steinberg::Vst::ParameterInfo& info)
{
    // Check if it's read-only (meter, status display, etc.)
    if (info.flags & Steinberg::Vst::ParameterInfo::kIsReadOnly) {
        return ParamType::ReadOnly;
    }

    // Check if it's a list/dropdown parameter
    if (info.flags & Steinberg::Vst::ParameterInfo::kIsList) {
        return ParamType::Dropdown;
    }

    // Check step count to determine type
    if (info.stepCount == 1) {
        // Binary on/off parameter
        return ParamType::Toggle;
    } else if (info.stepCount > 1) {
        // Discrete parameter with multiple steps
        return ParamType::Dropdown;
    }

    // Continuous parameter - could be slider or numeric input
    // For now, default to slider for continuous parameters
    return ParamType::Slider;
}

//! Get enum values for a list parameter
void getEnumValues(Steinberg::Vst::IEditController* controller,
                   const Steinberg::Vst::ParameterInfo& info,
                   ParamInfo& paramInfo)
{
    if (info.stepCount <= 0) {
        return;
    }

    try {
        // Reserve space to avoid reallocations
        paramInfo.enumValues.reserve(info.stepCount + 1);
        paramInfo.enumIndices.reserve(info.stepCount + 1);

        // For list parameters, try to get the string representations
        for (Steinberg::int32 i = 0; i <= info.stepCount; ++i) {
            Steinberg::Vst::String128 stringValue;
            double normalizedValue = static_cast<double>(i) / static_cast<double>(info.stepCount);

            if (controller->getParamStringByValue(info.id, normalizedValue, stringValue) == Steinberg::kResultOk) {
                paramInfo.enumValues.push_back(VST3Utils::UTF16ToStdString(stringValue));
                paramInfo.enumIndices.push_back(normalizedValue);
            } else {
                // Fallback: use numeric representation
                paramInfo.enumValues.push_back(std::to_string(i));
                paramInfo.enumIndices.push_back(normalizedValue);
            }
        }
    } catch (const std::exception& e) {
        wxLogDebug("VST3ParameterExtraction: exception getting enum values for param %d: %s",
                   info.id, e.what());
    }
}
} // anonymous namespace

std::vector<ParamInfo> extractParameters(EffectInstanceEx* instance, EffectSettingsAccess* settingsAccess)
{
    if (!instance) {
        return {};
    }

    // Try to cast to VST3Instance
    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return {};
    }

    auto& wrapper = vst3Instance->GetWrapper();

    // Only call FetchSettings if settings have changed since last time
    // This avoids expensive state restoration on every parameter read
    if (settingsAccess) {
        const uint64_t currentCounter = settingsAccess->modificationCounter();
        if (currentCounter != wrapper.lastFetchedSettingsCounter) {
            settingsAccess->ModifySettings([&wrapper](EffectSettings& settings) {
                // FetchSettings applies the stored processor/controller state to the edit controller
                // resetState=false means don't reset to defaults if no stored state exists
                wrapper.FetchSettings(settings, false);
                return nullptr;
            });
            wrapper.lastFetchedSettingsCounter = currentCounter;
        }
    }

    // Get the edit controller
    auto editController = vst3Instance->vstEditController();
    if (!editController) {
        return {};
    }

    std::vector<ParamInfo> result;

    try {
        const Steinberg::int32 paramCount = editController->getParameterCount();

        for (Steinberg::int32 i = 0; i < paramCount; ++i) {
            try {
                Steinberg::Vst::ParameterInfo vst3Info;
                if (editController->getParameterInfo(i, vst3Info) != Steinberg::kResultOk) {
                    continue;
                }

                // Skip hidden parameters
                if (vst3Info.flags & Steinberg::Vst::ParameterInfo::kIsHidden) {
                    continue;
                }

                // Create parameter info
                ParamInfo paramInfo;

                // Basic info
                paramInfo.id = vst3Info.id;
                paramInfo.name = VST3Utils::UTF16ToStdString(vst3Info.title);

                // Get units
                paramInfo.units = VST3Utils::GetParameterUnitStdString(editController.get(), vst3Info);

                // Type and flags
                paramInfo.type = getParameterType(vst3Info);
                paramInfo.isReadOnly = (vst3Info.flags & Steinberg::Vst::ParameterInfo::kIsReadOnly) != 0;
                // Note: isHidden is always false here since hidden parameters are filtered above
                paramInfo.canAutomate = (vst3Info.flags & Steinberg::Vst::ParameterInfo::kCanAutomate) != 0;

                // Step count
                paramInfo.stepCount = vst3Info.stepCount;
                if (paramInfo.stepCount > 0) {
                    paramInfo.stepSize = 1.0 / static_cast<double>(paramInfo.stepCount);
                }

                // Get normalized values first
                double normalizedDefault = vst3Info.defaultNormalizedValue;
                double normalizedCurrent = editController->getParamNormalized(vst3Info.id);

                // Get formatted value string from plugin (e.g., "440 Hz", "3.5 dB")
                Steinberg::Vst::String128 stringValue;
                if (editController->getParamStringByValue(vst3Info.id, normalizedCurrent, stringValue) == Steinberg::kResultOk) {
                    paramInfo.currentValueString = VST3Utils::UTF16ToStdString(stringValue);
                }

                // Convert to plain (display) values via normalizedParamToPlain
                // For plugins that don't implement proper conversions, these will be 0.0 to 1.0
                try {
                    paramInfo.minValue = editController->normalizedParamToPlain(vst3Info.id, 0.0);
                    paramInfo.maxValue = editController->normalizedParamToPlain(vst3Info.id, 1.0);
                    paramInfo.defaultValue = editController->normalizedParamToPlain(vst3Info.id, normalizedDefault);
                    paramInfo.currentValue = editController->normalizedParamToPlain(vst3Info.id, normalizedCurrent);
                } catch (...) {
                    // Fallback to normalized values if conversion fails
                    paramInfo.minValue = 0.0;
                    paramInfo.maxValue = 1.0;
                    paramInfo.defaultValue = normalizedDefault;
                    paramInfo.currentValue = normalizedCurrent;
                }

                // For list/dropdown parameters, get the enum values
                if (paramInfo.type == ParamType::Dropdown && paramInfo.stepCount > 0) {
                    getEnumValues(editController.get(), vst3Info, paramInfo);
                }

                result.push_back(paramInfo);
            } catch (const std::exception& e) {
                wxLogDebug("VST3ParameterExtraction: exception processing param %d: %s", i, e.what());
            }
        }
    } catch (const std::exception& e) {
        wxLogDebug("VST3ParameterExtraction: exception extracting parameters: %s", e.what());
    }

    return result;
}

double getParameterValue(EffectInstanceEx* instance, uint32_t parameterId)
{
    if (!instance) {
        return 0.0;
    }

    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return 0.0;
    }

    auto editController = vst3Instance->vstEditController();
    if (!editController) {
        return 0.0;
    }

    return editController->getParamNormalized(parameterId);
}

bool setParameterValue(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue,
                       EffectSettingsAccess* settingsAccess)
{
    if (!instance) {
        return false;
    }

    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return false;
    }

    auto& wrapper = vst3Instance->GetWrapper();
    auto editController = wrapper.mEditController;
    if (!editController) {
        return false;
    }

    // Clamp to normalized [0.0, 1.0] range for VST3 API
    normalizedValue = std::clamp(normalizedValue, 0.0, 1.0);

    // Set the parameter value on the edit controller
    if (editController->setParamNormalized(parameterId, normalizedValue) != Steinberg::kResultOk) {
        return false;
    }

    // Also notify via component handler to update settings and sync with audio processor
    // This is the same flow used by the vendor UI when parameters are changed
    if (wrapper.mComponentHandler) {
        wrapper.mComponentHandler->beginEdit(parameterId);
        wrapper.mComponentHandler->performEdit(parameterId, normalizedValue);
        wrapper.mComponentHandler->endEdit(parameterId);
    }

    // If settings access is provided, flush parameters and store settings to persist the change
    // This ensures the parameter change is saved and will be restored when switching UI modes
    if (settingsAccess) {
        settingsAccess->ModifySettings([&wrapper](EffectSettings& settings) {
            wrapper.FlushParameters(settings);
            wrapper.StoreSettings(settings);
            return nullptr;
        });
        settingsAccess->Flush();
    }

    return true;
}

std::string getParameterValueString(EffectInstanceEx* instance, uint32_t parameterId, double value)
{
    if (!instance) {
        return std::string();
    }

    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return std::string();
    }

    auto editController = vst3Instance->vstEditController();
    if (!editController) {
        return std::string();
    }

    // Get the formatted string from VST3
    Steinberg::Vst::String128 stringValue;
    if (editController->getParamStringByValue(parameterId, value, stringValue) == Steinberg::kResultOk) {
        return VST3Utils::UTF16ToStdString(stringValue);
    }

    // Fallback: simple numeric formatting
    std::ostringstream oss;
    oss << value;
    return oss.str();
}

double normalizedToPlain(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue)
{
    if (!instance) {
        return normalizedValue;
    }

    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return normalizedValue;
    }

    auto editController = vst3Instance->vstEditController();
    if (!editController) {
        return normalizedValue;
    }

    try {
        return editController->normalizedParamToPlain(parameterId, normalizedValue);
    } catch (const std::exception& e) {
        wxLogDebug("VST3ParameterExtraction: exception in normalizedToPlain for param %u: %s",
                   parameterId, e.what());
        return normalizedValue;
    } catch (...) {
        wxLogDebug("VST3ParameterExtraction: unknown exception in normalizedToPlain for param %u",
                   parameterId);
        return normalizedValue;
    }
}

double plainToNormalized(EffectInstanceEx* instance, uint32_t parameterId, double plainValue)
{
    if (!instance) {
        return std::clamp(plainValue, 0.0, 1.0);
    }

    auto vst3Instance = dynamic_cast<VST3Instance*>(instance);
    if (!vst3Instance) {
        return std::clamp(plainValue, 0.0, 1.0);
    }

    auto editController = vst3Instance->vstEditController();
    if (!editController) {
        return std::clamp(plainValue, 0.0, 1.0);
    }

    try {
        double normalized = editController->plainParamToNormalized(parameterId, plainValue);
        return std::clamp(normalized, 0.0, 1.0);
    } catch (const std::exception& e) {
        wxLogDebug("VST3ParameterExtraction: exception in plainToNormalized for param %u: %s",
                   parameterId, e.what());
        return std::clamp(plainValue, 0.0, 1.0);
    } catch (...) {
        wxLogDebug("VST3ParameterExtraction: unknown exception in plainToNormalized for param %u",
                   parameterId);
        return std::clamp(plainValue, 0.0, 1.0);
    }
}
} // namespace VST3ParameterExtraction
