/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/effectstypes.h"

class EffectInstanceEx;
class EffectSettingsAccess;

namespace au::effects {
//! Helper class to extract parameters from VST3 plugins
//! This is in a separate compilation unit to avoid including VST3 SDK headers
//! in the main EffectParametersProvider implementation
class VST3ParametersExtractor
{
public:
    //! Extract all parameters from a VST3 effect instance
    //! @param settingsAccess Optional settings access to apply stored settings before extraction
    static ParameterInfoList extractParameters(EffectInstanceEx* instance,
                                               const std::shared_ptr<EffectSettingsAccess>& settingsAccess = nullptr);

    //! Get a single parameter by ID
    static ParameterInfo getParameter(EffectInstanceEx* instance, const muse::String& parameterId);

    //! Get the current value of a VST3 parameter
    static double getParameterValue(EffectInstanceEx* instance, const muse::String& parameterId);

    //! Set the value of a VST3 parameter
    //! @param settingsAccess Optional settings access to persist the change
    static bool setParameterValue(EffectInstanceEx* instance, const muse::String& parameterId, double value,
                                  const std::shared_ptr<EffectSettingsAccess>& settingsAccess = nullptr);

    //! Get formatted string representation of a VST3 parameter value
    static muse::String getParameterValueString(EffectInstanceEx* instance, const muse::String& parameterId, double value);
};
} // namespace au::effects
