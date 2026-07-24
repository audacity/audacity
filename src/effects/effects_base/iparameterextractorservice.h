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
    virtual ParameterInfoList extractParameters(EffectInstance* instance) const = 0;

    //! Get a single parameter by ID
    //! @return ParameterInfo for the parameter, or invalid ParameterInfo if not found
    virtual ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const = 0;

    //! Get the current value of a parameter
    virtual double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const = 0;

    //! Set the value of a parameter
    //! @param instance The effect instance
    //! @param parameterId The parameter identifier
    //! @param fullRangeValue Value in plain/"Full Range" units (implementation converts to plugin-specific format)
    virtual bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double fullRangeValue) = 0;

    //! Set the string value of a parameter (for file paths, text, etc.)
    //! @param instance The effect instance
    //! @param parameterId The parameter identifier
    //! @param stringValue The new string value (e.g., file path)
    //! @return true if successful, false if parameter doesn't support string values
    virtual bool setParameterStringValue([[maybe_unused]] EffectInstance* instance,
                                         [[maybe_unused]] const muse::String& parameterId,
                                         [[maybe_unused]] const muse::String& stringValue)
    {
        // Default implementation: not supported
        return false;
    }

    //! Get formatted string representation of a parameter value
    virtual muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const = 0;

    //! Begin parameter gesture (e.g., user starts dragging a slider)
    //! Called when interactive editing of a parameter begins
    //! @param instance The effect instance
    //! @param parameterId The parameter being edited
    virtual void beginParameterGesture([[maybe_unused]] EffectInstance* instance,
                                       [[maybe_unused]] const muse::String& parameterId)
    {
    }

    //! End parameter gesture (e.g., user releases a slider)
    //! Called when interactive editing of a parameter ends - this is when state should be saved
    //! @param instance The effect instance
    //! @param parameterId The parameter that was being edited
    virtual void endParameterGesture([[maybe_unused]] EffectInstance* instance,
                                     [[maybe_unused]] const muse::String& parameterId)
    {
    }

    //! Notify that an instance is being destroyed
    //! Called when an instance is unregistered to allow cleanup of cached data
    //! @param instance The effect instance being destroyed
    virtual void onInstanceDestroyed([[maybe_unused]] EffectInstance* instance)
    {
    }

    //! Begin parameter editing session
    //! Called before showing UI to set up parameter change tracking
    //! Sets up the ComponentHandler to track parameter changes for the given instance
    //! @param instance The effect instance
    virtual void beginParameterEditing([[maybe_unused]] EffectInstance* instance)
    {
    }

    //! End parameter editing session
    //! Called when closing UI to clean up parameter change tracking
    //! @param instance The effect instance
    virtual void endParameterEditing([[maybe_unused]] EffectInstance* instance)
    {
    }
};

using IParameterExtractorServicePtr = std::shared_ptr<IParameterExtractorService>;
}
