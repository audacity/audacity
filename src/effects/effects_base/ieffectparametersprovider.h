/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/channel.h"
#include "framework/global/modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
//! Data sent when a parameter value changes
struct ParameterChangedData {
    EffectInstanceId instanceId = -1;
    muse::String parameterId;
    double newFullRangeValue = 0.0;       // New value in "full range"/display units
    muse::String newValueString;      // Formatted string representation
};

//! Interface for extracting parameter metadata from effects
//! This provides a unified way to get parameter information from different plugin formats
//! (VST3, LV2, Audio Units, etc.) for auto-generating UIs
class IEffectParametersProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectParametersProvider)

public:
    virtual ~IEffectParametersProvider() = default;

    //! Get all parameters for an effect instance
    //! @param instanceId The effect instance ID
    //! @return List of parameter metadata, or empty list if not available
    virtual ParameterInfoList parameters(EffectInstanceId instanceId) const = 0;

    //! Get a specific parameter by ID
    //! @param instanceId The effect instance ID
    //! @param parameterId The parameter identifier
    //! @return Parameter metadata, or invalid ParameterInfo if not found
    virtual ParameterInfo parameter(EffectInstanceId instanceId, const muse::String& parameterId) const = 0;

    //! Get current value of a parameter
    //! @param instanceId The effect instance ID
    //! @param parameterId The parameter identifier
    //! @return Current parameter value
    virtual double parameterValue(EffectInstanceId instanceId, const muse::String& parameterId) const = 0;

    //! Set value of a parameter
    //! @param instanceId The effect instance ID
    //! @param parameterId The parameter identifier
    //! @param value The new value to set (normalized 0-1)
    //! @return true if successful
    virtual bool setParameterValue(EffectInstanceId instanceId, const muse::String& parameterId, double value) = 0;

    //! Get formatted string representation of a parameter value
    //! @param instanceId The effect instance ID
    //! @param parameterId The parameter identifier
    //! @param value The value to format
    //! @return Formatted string (e.g., "3.5 dB", "440 Hz")
    virtual muse::String parameterValueString(EffectInstanceId instanceId, const muse::String& parameterId, double value) const = 0;

    //! Check if an effect supports parameter extraction
    //! @param effectId The effect ID
    //! @return true if parameters can be extracted
    virtual bool supportsParameterExtraction(const EffectId& effectId) const = 0;

    //! Channel that sends data when a specific parameter value changes
    //! Subscribers receive the instance ID, parameter ID, and new values
    virtual muse::async::Channel<ParameterChangedData> parameterChanged() const = 0;
};
}
