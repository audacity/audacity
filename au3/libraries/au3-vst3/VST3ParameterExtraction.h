/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3ParameterExtraction.h

  @brief Bridge for extracting VST3 parameters without exposing VST3 SDK types

**********************************************************************/

#pragma once

#include <cstdint>
#include <vector>
#include <string>

class EffectInstanceEx;
class EffectSettingsAccess;

namespace VST3ParameterExtraction {
//! Parameter type enumeration (mirrors AU4's ParameterType)
enum class ParamType {
    Unknown = -1,
    Toggle,        // Boolean on/off
    Dropdown,      // Enumerated list of choices
    Slider,        // Continuous value with range
    Numeric,       // Numeric input field
    ReadOnly,      // Display-only (meter, status)
};

//! Simple parameter info structure that doesn't expose VST3 SDK types
//! Values are stored in "Full Range" (display) representation, obtained via normalizedParamToFullRange().
//! Use getNormalizedValue() to convert back to normalized [0,1] for VST3 API calls.
struct ParamInfo {
    uint32_t id = 0;                  // VST3 parameter ID (matches Steinberg::Vst::ParamID type)
    std::string name;                 // Display name
    std::string description;          // Optional description
    std::string units;                // Unit string (dB, Hz, %, etc.)
    std::string group;                // Parameter group/category

    ParamType type = ParamType::Unknown;

    // Value range in "Full Range" (display) values, e.g., -60 to +6 for dB, 20 to 20000 for Hz
    // Obtained via normalizedParamToFullRange(). For plugins that don't implement proper
    // conversions, these will be 0.0 to 1.0 (same as normalized).
    double minValue = 0.0;
    double maxValue = 0.0;
    double defaultValue = 0.0;
    double currentValue = 0.0;

    // Formatted value string from plugin (e.g., "440 Hz", "3.5 dB", "-12.0 dB")
    // This is the plugin's own string representation via getParamStringByValue()
    std::string currentValueString;

    // For discrete parameters
    int stepCount = 0;                // 0=continuous, 1=toggle, >1=discrete steps
    double stepSize = 0.0;            // Step increment for discrete values

    // For dropdown/enumeration parameters
    std::vector<std::string> enumValues;  // List of choice labels
    std::vector<double> enumIndices;      // Corresponding normalized values

    // Flags
    bool isReadOnly = false;
    bool isHidden = false;
    bool isLogarithmic = false;
    bool isInteger = false;
    bool canAutomate = true;

    //! Convert current "Full Range" value to normalized [0,1] for VST3 API calls
    double getNormalizedValue() const
    {
        if (maxValue == minValue) {
            return 0.0; // Avoid division by zero
        }
        return (currentValue - minValue) / (maxValue - minValue);
    }

    //! Convert a "Full Range" value to normalized [0,1]
    double toNormalized(double fullRangeValue) const
    {
        if (maxValue == minValue) {
            return 0.0;
        }
        return (fullRangeValue - minValue) / (maxValue - minValue);
    }

    //! Convert a normalized [0,1] value to fullRange
    double toFullRange(double normalizedValue) const
    {
        return minValue + normalizedValue * (maxValue - minValue);
    }
};

//! Extract all parameters from a VST3 effect instance
//! @param settingsAccess Optional settings access to apply stored settings before extraction
//! Returns empty vector if instance is not a VST3 instance or on error
std::vector<ParamInfo> extractParameters(EffectInstanceEx* instance, EffectSettingsAccess* settingsAccess = nullptr);

//! Get a single parameter by ID
//! Returns default ParamInfo with id=0 if parameter not found or on error
ParamInfo getParameter(EffectInstanceEx* instance, uint32_t parameterId);

//! Get the current normalized value [0,1] of a VST3 parameter
//! Returns 0.0 if parameter not found or on error
double getParameterValue(EffectInstanceEx* instance, uint32_t parameterId);

//! Set the value of a VST3 parameter using a normalized value [0,1]
//! Use ParamInfo::toNormalized() to convert "Full Range" values before calling this
//! @param normalizedValue Value in [0,1] range
//! @param settingsAccess Optional settings access to persist the change
//! Returns true on success, false on error
bool setParameterValue(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue,
                       EffectSettingsAccess* settingsAccess = nullptr);

//! Get formatted string representation of a VST3 parameter value
//! @param normalizedValue Value in [0,1] range
//! Returns empty string if parameter not found or on error
std::string getParameterValueString(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue);

//! Convert normalized value (0.0-1.0) to "Full Range" value (actual display value)
//! Returns the "Full Range" value, or the normalized value if conversion is not supported
double normalizedToFullRange(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue);

//! Convert "Full Range" value (actual display value) to normalized value (0.0-1.0)
//! Returns the normalized value, or the "Full Range" value clamped to 0.0-1.0 if conversion is not supported
double fullRangeToNormalized(EffectInstanceEx* instance, uint32_t parameterId, double fullRangeValue);

//! Begin edit gesture for a single parameter (e.g., user starts dragging slider)
//! Calls VST3 IComponentHandler::beginEdit()
//! @param instance The VST3 effect instance
//! @param parameterId The parameter ID
void beginEdit(EffectInstanceEx* instance, uint32_t parameterId);

//! End edit gesture for a single parameter (e.g., user releases slider)
//! Calls VST3 IComponentHandler::endEdit()
//! @param instance The VST3 effect instance
//! @param parameterId The parameter ID
void endEdit(EffectInstanceEx* instance, uint32_t parameterId);

//! Flush parameters and store settings
//! Should be called after endEdit to persist the final state
//! @param instance The VST3 effect instance
//! @param settingsAccess Settings access to use for storing
void flushAndStoreSettings(EffectInstanceEx* instance, EffectSettingsAccess* settingsAccess);

//! Begin parameter editing session
//! Sets up the ComponentHandler to track parameter changes with the given settings access
//! Should be called once when opening the effect UI
//! @param instance The VST3 effect instance
//! @param settingsAccess Settings access to use for parameter changes during the session
void beginParameterEditing(EffectInstanceEx* instance, EffectSettingsAccess* settingsAccess);

//! End parameter editing session
//! Clears the ComponentHandler's settings access pointer
//! Should be called once when closing the effect UI
//! @param instance The VST3 effect instance
void endParameterEditing(EffectInstanceEx* instance);
} // namespace VST3ParameterExtraction
