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
struct ParamInfo {
    uint32_t id = 0;                  // VST3 parameter ID (matches Steinberg::Vst::ParamID type)
    std::string name;                 // Display name
    std::string description;          // Optional description
    std::string units;                // Unit string (dB, Hz, %, etc.)
    std::string group;                // Parameter group/category

    ParamType type = ParamType::Unknown;

    // Value range (normalized 0.0 to 1.0)
    double minValue = 0.0;
    double maxValue = 1.0;
    double defaultValue = 0.0;
    double currentValue = 0.0;

    // Formatted value string from plugin (e.g., "440 Hz", "3.5 dB", "-12.0 dB")
    // This is the plugin's own string representation via getParamStringByValue()
    std::string currentValueString;

    // Plain value range (actual display values for DISCRETE parameters only)
    // For continuous parameters, these are NOT reliable - use currentValueString instead
    // Only valid if hasPlainRange is true (discrete parameters with working conversions)
    double plainMinValue = 0.0;
    double plainMaxValue = 1.0;
    double plainDefaultValue = 0.0;
    double plainCurrentValue = 0.0;
    bool hasPlainRange = false;  // True for discrete params with valid plain value conversions

    // For discrete parameters
    int stepCount = 0;                // 0=continuous, 1=toggle, >1=discrete steps
    double stepSize = 0.0;            // Step increment for discrete values

    // For dropdown/enumeration parameters
    std::vector<std::string> enumValues;  // List of choice labels
    std::vector<double> enumIndices;      // Corresponding numeric values

    // Flags
    bool isReadOnly = false;
    bool isHidden = false;
    bool isLogarithmic = false;
    bool isInteger = false;
    bool canAutomate = true;
};

//! Extract all parameters from a VST3 effect instance
//! Returns empty vector if instance is not a VST3 instance or on error
std::vector<ParamInfo> extractParameters(EffectInstanceEx* instance);

//! Get the current value of a VST3 parameter
//! Returns 0.0 if parameter not found or on error
double getParameterValue(EffectInstanceEx* instance, uint32_t parameterId);

//! Set the value of a VST3 parameter
//! @param settingsAccess Optional settings access to persist the change
//! Returns true on success, false on error
bool setParameterValue(EffectInstanceEx* instance, uint32_t parameterId, double value, EffectSettingsAccess* settingsAccess = nullptr);

//! Get formatted string representation of a VST3 parameter value
//! Returns empty string if parameter not found or on error
std::string getParameterValueString(EffectInstanceEx* instance, uint32_t parameterId, double value);

//! Convert normalized value (0.0-1.0) to plain value (actual display value)
//! Returns the plain value, or the normalized value if conversion is not supported
double normalizedToPlain(EffectInstanceEx* instance, uint32_t parameterId, double normalizedValue);

//! Convert plain value (actual display value) to normalized value (0.0-1.0)
//! Returns the normalized value, or the plain value clamped to 0.0-1.0 if conversion is not supported
double plainToNormalized(EffectInstanceEx* instance, uint32_t parameterId, double plainValue);
} // namespace VST3ParameterExtraction
