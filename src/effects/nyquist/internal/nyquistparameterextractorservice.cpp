/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistparameterextractorservice.h"

#include <cmath>

#include "au3wrap/internal/wxtypes_convert.h"

// AU3 Nyquist effect base class
#include "au3-nyquist-effects/NyquistBase.h"
#include "au3-effects/StatefulEffectBase.h"

using namespace au::effects;
using namespace muse;

namespace {
//! Convert NyqControlType to AU4 ParameterType
ParameterType convertControlType(int nyqType)
{
    switch (nyqType) {
    case NYQ_CTRL_INT_TEXT:
    case NYQ_CTRL_FLOAT_TEXT:
        // `int-text` and `float-text` renders as a plain numeric input (no slider)
        // to match legacy AU3 semantics (au3/src/effects/nyquist/Nyquist.cpp:487-522)
        // and to avoid creating an unusable slider when the high bound is
        // `nil` (parsed as FLT_MAX in NyquistBase.cpp).
        return ParameterType::Numeric;
    case NYQ_CTRL_INT:
    case NYQ_CTRL_FLOAT:
        return ParameterType::Slider;
    case NYQ_CTRL_CHOICE:
        return ParameterType::Dropdown;
    case NYQ_CTRL_STRING:
        return ParameterType::Text; // Free-form text input field
    case NYQ_CTRL_TIME:
        return ParameterType::Time;
    case NYQ_CTRL_TEXT: // Informational text, not editable
        return ParameterType::ReadOnly;
    case NYQ_CTRL_FILE:
        return ParameterType::File;
    default:
        return ParameterType::Unknown;
    }
}

//! Convert NyqControl to AU4 ParameterInfo
ParameterInfo convertControl(const NyqControl& ctrl)
{
    ParameterInfo info;

    // Use variable name as ID
    info.id = String::fromStdString(au::au3::wxToStdString(ctrl.var));
    info.name = String::fromStdString(au::au3::wxToStdString(ctrl.name));
    // Nyquist's ctrl.label is a freeform descriptor (e.g. "30 - 300 beats/minute"),
    // not a unit symbol. Map it to `description`; leave `units` empty.
    info.description = String::fromStdString(au::au3::wxToStdString(ctrl.label));

    info.type = convertControlType(ctrl.type);

    // Value range
    info.minValue = ctrl.low;
    info.maxValue = ctrl.high;
    info.defaultValue = ctrl.val;
    info.currentValue = ctrl.val;

    // For integer controls
    if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
        info.isInteger = true;
        info.stepSize = 1.0;
    } else if (ctrl.ticks > 0) {
        // Calculate step size from ticks.
        // Nyquist `float-text` / `time` controls with a `nil` bound are parsed
        // as ±FLT_MAX in NyquistBase.cpp, and ticks is hard-coded to 1000
        // there. Refuse to synthesise a stepSize from a non-finite or
        // absurdly wide range: leave stepSize=0 so the QML fallback
        // (step=0.01 in GeneratedIncrementalPropertyControl.qml) kicks in.
        const double range = ctrl.high - ctrl.low;
        if (std::isfinite(range) && range > 0 && range < 1e15) {
            info.stepSize = range / ctrl.ticks;
            info.stepCount = ctrl.ticks;
        }
    }

    // For choice controls, extract enum values
    if (ctrl.type == NYQ_CTRL_CHOICE) {
        info.enumValues.reserve(ctrl.choices.size());
        info.enumIndices.reserve(ctrl.choices.size());

        for (size_t i = 0; i < ctrl.choices.size(); ++i) {
            const auto& choice = ctrl.choices[i];
            info.enumValues.push_back(String::fromStdString(choice.Msgid().Translation().ToStdString()));
            info.enumIndices.push_back(static_cast<double>(i));
        }

        // Update min/max to match the enum indices range
        if (!ctrl.choices.empty()) {
            info.minValue = 0.0;
            info.maxValue = static_cast<double>(ctrl.choices.size() - 1);
        }
    }

    // For file controls, extract file type filters and parse flags
    if (ctrl.type == NYQ_CTRL_FILE) {
        info.fileFilters.reserve(ctrl.fileTypes.size());

        for (const auto& fileType : ctrl.fileTypes) {
            // Convert FileType to filter string format: "Description (*.ext1 *.ext2)"
            wxString filterStr = fileType.description.Translation();

            if (!fileType.extensions.empty()) {
                wxString extList;
                for (const auto& ext : fileType.extensions) {
                    if (!extList.empty()) {
                        extList += wxT(" ");
                    }
                    if (ext.empty()) {
                        // Empty extension means "all files"
                        extList += wxT("*");
                    } else {
                        extList += wxT("*.") + ext;
                    }
                }
                filterStr += wxT(" (") + extList + wxT(")");
            }

            info.fileFilters.push_back(String::fromStdString(au::au3::wxToStdString(filterStr)));
        }

        // Parse file control flags from highStr (e.g., "open,exists,multiple" or "save,overwrite")
        wxString flags = ctrl.highStr.Lower();
        info.isFileSave = flags.Contains(wxT("save"));
        info.isFileMultiple = flags.Contains(wxT("multiple"));

        // Set currentValueString from valStr for file controls
        info.currentValueString = String::fromStdString(au::au3::wxToStdString(ctrl.valStr));
    }

    // For string controls, set currentValueString from valStr
    if (ctrl.type == NYQ_CTRL_STRING) {
        info.currentValueString = String::fromStdString(au::au3::wxToStdString(ctrl.valStr));
    }

    // For float/slider controls, format the current value as string
    // This is needed for the formattedValue display next to the slider in the UI
    if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT) {
        // Use reasonable precision: 2 decimal places for most cases
        // Could be made more sophisticated based on the range/stepSize in the future
        info.currentValueString = String::number(ctrl.val, 2);
    }

    return info;
}

//! Get NyquistBase effect from EffectInstance
NyquistBase* getNyquistBase(::EffectInstance* instance)
{
    if (!instance) {
        return nullptr;
    }

    // The instance is a StatefulEffectBase::Instance, not the NyquistBase itself
    // We need to get the effect from the instance
    auto* statefulInstance = dynamic_cast<StatefulEffectBase::Instance*>(instance);
    if (!statefulInstance) {
        return nullptr;
    }

    // Get the effect and cast to NyquistBase
    return dynamic_cast<NyquistBase*>(&statefulInstance->GetEffect());
}

//! Find control by variable name (const version)
const NyqControl* findControl(const std::vector<NyqControl>& controls, const String& varName)
{
    const std::string varNameStd = varName.toStdString();
    for (const auto& ctrl : controls) {
        if (au::au3::wxToStdString(ctrl.var) == varNameStd) {
            return &ctrl;
        }
    }
    return nullptr;
}

//! Find control by variable name (non-const version)
//! Implemented by calling the const version and casting away constness
NyqControl* findControl(std::vector<NyqControl>& controls, const String& varName)
{
    // Call the const version and cast away constness
    // This is safe because we know the original vector is non-const
    return const_cast<NyqControl*>(findControl(const_cast<const std::vector<NyqControl>&>(controls), varName));
}
} // anonymous namespace

ParameterInfoList NyquistParameterExtractorService::extractParameters(EffectInstance* instance,
                                                                      [[maybe_unused]] EffectSettingsAccessPtr settingsAccess) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return {};
    }

    ParameterInfoList result;
    result.reserve(nyquist->mControls.size());

    for (const auto& ctrl : nyquist->mControls) {
        result.push_back(convertControl(ctrl));
    }

    return result;
}

ParameterInfo NyquistParameterExtractorService::getParameter(EffectInstance* instance, const String& parameterId) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return {};
    }

    const NyqControl* ctrl = findControl(nyquist->mControls, parameterId);
    if (!ctrl) {
        return {};
    }

    return convertControl(*ctrl);
}

double NyquistParameterExtractorService::getParameterValue(EffectInstance* instance, const String& parameterId) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return 0.0;
    }

    const NyqControl* ctrl = findControl(nyquist->mControls, parameterId);
    if (!ctrl) {
        return 0.0;
    }

    return ctrl->val;
}

bool NyquistParameterExtractorService::setParameterValue(EffectInstance* instance, const String& parameterId,
                                                         double fullRangeValue, EffectSettingsAccessPtr settingsAccess)
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return false;
    }

    NyqControl* ctrl = findControl(nyquist->mControls, parameterId);
    if (!ctrl) {
        return false;
    }

    // For choice controls, use the number of choices to determine valid range
    // (ctrl->low and ctrl->high are 0 for choice controls)
    if (ctrl->type == NYQ_CTRL_CHOICE) {
        const double maxChoice = static_cast<double>(ctrl->choices.size() - 1);
        ctrl->val = std::max(0.0, std::min(maxChoice, fullRangeValue));
    } else {
        // For other controls, clamp to the specified range
        ctrl->val = std::max(ctrl->low, std::min(ctrl->high, fullRangeValue));
    }

    // Update string representation
    ctrl->valStr = wxString::Format(wxT("%g"), ctrl->val);

    // Sync mControls back to EffectSettings for preset saving
    // This ensures that when SaveUserPreset() is called, it has the updated control values
    if (settingsAccess) {
        settingsAccess->ModifySettings([&](EffectSettings& settings) {
            NyquistBase::GetSettings(settings).controls = nyquist->mControls;
            return nullptr;
        });
    }

    return true;
}

bool NyquistParameterExtractorService::setParameterStringValue(EffectInstance* instance, const String& parameterId,
                                                               const String& stringValue, EffectSettingsAccessPtr settingsAccess)
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return false;
    }

    NyqControl* ctrl = findControl(nyquist->mControls, parameterId);
    if (!ctrl) {
        return false;
    }

    // Only file and string parameters support string values
    if (ctrl->type != NYQ_CTRL_FILE && ctrl->type != NYQ_CTRL_STRING) {
        return false;
    }

    // Set the string value directly
    ctrl->valStr = au3::wxFromString(stringValue);

    // Sync mControls back to EffectSettings for preset saving
    if (settingsAccess) {
        settingsAccess->ModifySettings([&](EffectSettings& settings) {
            NyquistBase::GetSettings(settings).controls = nyquist->mControls;
            return nullptr;
        });
    }

    return true;
}

muse::String NyquistParameterExtractorService::getParameterValueString(EffectInstance* instance,
                                                                       const String& parameterId, double value) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return String();
    }

    const NyqControl* ctrl = findControl(nyquist->mControls, parameterId);
    if (!ctrl) {
        return String();
    }

    // Format based on control type
    switch (ctrl->type) {
    case NYQ_CTRL_INT:
    case NYQ_CTRL_INT_TEXT:
        return String::number(static_cast<int>(value));

    case NYQ_CTRL_CHOICE:
    {
        // Return the choice label for the given index
        int index = static_cast<int>(value);
        if (index >= 0 && index < static_cast<int>(ctrl->choices.size())) {
            return String::fromStdString(ctrl->choices[index].Msgid().Translation().ToStdString());
        }
        return String::number(index);
    }

    case NYQ_CTRL_TIME:
        // TODO: Format as time (HH:MM:SS or similar)
        // For now, just return the numeric value
        return String::number(value, 3);

    case NYQ_CTRL_FILE:
        // Return the file path from valStr
        return String::fromStdString(au3::wxToStdString(ctrl->valStr));

    case NYQ_CTRL_STRING:
        // Return the string value from valStr
        return String::fromStdString(au3::wxToStdString(ctrl->valStr));

    case NYQ_CTRL_TEXT:
        // Return the informational text from name field (read-only display)
        return String::fromStdString(au3::wxToStdString(ctrl->name));

    default:
        // For numeric types, format with appropriate precision
        if (ctrl->type == NYQ_CTRL_FLOAT || ctrl->type == NYQ_CTRL_FLOAT_TEXT) {
            // Use reasonable precision: 2 decimal places for most cases
            // Could be made more sophisticated based on the range/stepSize in the future
            return String::number(value, 2);
        }
        return String::number(value);
    }
}

muse::String NyquistParameterExtractorService::getPromptCommandText(EffectInstance* instance) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist || !nyquist->mIsPrompt) {
        return String();
    }

    return String::fromStdString(au3::wxToStdString(nyquist->mInputCmd));
}

bool NyquistParameterExtractorService::setPromptCommandText(EffectInstance* instance, const String& commandText,
                                                            [[maybe_unused]] EffectSettingsAccessPtr settingsAccess)
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist || !nyquist->mIsPrompt) {
        return false;
    }

    // Set the command text directly on the NyquistBase instance
    nyquist->mInputCmd = au3::wxFromString(commandText);

    return true;
}

void NyquistParameterExtractorService::setDebugMode(EffectInstance* instance, bool enable)
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return;
    }

    nyquist->mDebug = enable;
    // Note: Do NOT call RedirectOutput() here!
    // When mRedirectOutput is false, OutputCallback captures to mDebugOutputStr
    // When mRedirectOutput is true, OutputCallback prints to std::cout
    // We want to capture to mDebugOutputStr, so we leave mRedirectOutput as false
}

muse::String NyquistParameterExtractorService::getDebugOutput(EffectInstance* instance) const
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return String();
    }

    return String::fromStdString(au3::wxToStdString(nyquist->mDebugOutputStr));
}

muse::String NyquistParameterExtractorService::executeForDebug(EffectInstance* instance, EffectSettings& settings)
{
    NyquistBase* nyquist = getNyquistBase(instance);
    if (!nyquist) {
        return String();
    }

    // Use SetCommand to parse the current command text
    // This sets up mCmd, mIsSal, and other necessary state
    nyquist->SetCommand(nyquist->mInputCmd);

    // Enable debug mode
    nyquist->mDebug = true;
    // Clear previous debug output
    nyquist->mDebugOutputStr.clear();

    // Execute the Nyquist code by calling Process
    // This will run the code and capture output to mDebugOutputStr
    nyquist->Process(*instance, settings);

    // Get the captured output
    String debugOutput = String::fromStdString(au3::wxToStdString(nyquist->mDebugOutputStr));

    // Disable debug mode
    nyquist->mDebug = false;

    return debugOutput;
}
