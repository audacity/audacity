/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"

namespace au::effects {
//! Generic interface for parameter extraction from plugin effects.
//! Each plugin format (VST3, LV2, AudioUnit) provides its own implementation.
//! Implementations are registered with IParameterExtractorRegistry.
class IParameterExtractorService
{
public:
    virtual ~IParameterExtractorService() = default;

    //! Returns which effect family this extractor handles
    virtual EffectFamily family() const = 0;

    //! Extract all parameters from an effect instance
    //! @param instance The effect instance
    //! @param settingsAccess Optional settings access to apply stored settings before extraction
    virtual ParameterInfoList extractParameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess = nullptr) const = 0;

    //! Get the current value of a parameter
    virtual double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const = 0;

    //! Set the value of a parameter
    //! @param instance The effect instance
    //! @param parameterId The parameter identifier
    //! @param normalizedValue Value in [0,1] range
    //! @param settingsAccess Optional settings access to persist the change
    virtual bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double normalizedValue,
                                   EffectSettingsAccessPtr settingsAccess = nullptr) = 0;

    //! Get formatted string representation of a parameter value
    virtual muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const = 0;
};

using IParameterExtractorServicePtr = std::shared_ptr<IParameterExtractorService>;
}
