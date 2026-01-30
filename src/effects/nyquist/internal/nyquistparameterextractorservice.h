/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"

namespace au::effects {
//! Implementation of parameter extraction for Nyquist plugins.
//! Registered with IParameterExtractorRegistry for EffectFamily::Nyquist.
//! Extracts parameters from NyquistBase::mControls vector.
class NyquistParameterExtractorService : public IParameterExtractorService
{
public:
    EffectFamily family() const override { return EffectFamily::Nyquist; }

    ParameterInfoList extractParameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess = nullptr) const override;

    ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const override;

    double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const override;

    bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double fullRangeValue,
                           EffectSettingsAccessPtr settingsAccess = nullptr) override;

    bool setParameterStringValue(EffectInstance* instance, const muse::String& parameterId, const muse::String& stringValue,
                                 EffectSettingsAccessPtr settingsAccess = nullptr) override;

    muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const override;

    //! Check if the effect instance is the Nyquist Prompt
    //! @param instance The effect instance to check
    //! @return true if this is the Nyquist Prompt, false otherwise
    bool isNyquistPrompt(EffectInstance* instance) const;

    //! Get the Nyquist command text from the Nyquist Prompt
    //! @param instance The effect instance (must be Nyquist Prompt)
    //! @return The command text, or empty string if not a prompt
    muse::String getPromptCommandText(EffectInstance* instance) const;

    //! Set the Nyquist command text for the Nyquist Prompt
    //! @param instance The effect instance (must be Nyquist Prompt)
    //! @param commandText The Nyquist code to set
    //! @param settingsAccess Optional settings access to persist the change
    //! @return true if successful
    bool setPromptCommandText(EffectInstance* instance, const muse::String& commandText, EffectSettingsAccessPtr settingsAccess = nullptr);

    //! Enable debug mode for the Nyquist effect
    //! @param instance The effect instance
    //! @param enable true to enable debug mode, false to disable
    void setDebugMode(EffectInstance* instance, bool enable);

    //! Get the debug output from the last execution
    //! @param instance The effect instance
    //! @return The debug output text
    muse::String getDebugOutput(EffectInstance* instance) const;

    //! Execute the Nyquist code for debugging (without audio playback)
    //! @param instance The effect instance
    //! @param settings The effect settings
    //! @return The debug output text
    muse::String executeForDebug(EffectInstance* instance, EffectSettings& settings);
};
}
