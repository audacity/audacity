/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"

#include "au3-vst3/VST3ParameterExtraction.h" // Need full definition for std::unordered_map value type

#include <unordered_map>
#include <cstdint>

namespace au::effects {
//! Implementation of parameter extraction for VST3 plugins.
//! Registered with IParameterExtractorRegistry for EffectFamily::VST3.
//! Maintains a parameter cache for O(1) lookups after initial extraction.
class VstParameterExtractorService : public IParameterExtractorService
{
public:
    EffectFamily family() const override { return EffectFamily::VST3; }

    ParameterInfoList extractParameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess = nullptr) const override;

    ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const override;

    double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const override;

    bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double fullRangeValue,
                           EffectSettingsAccessPtr settingsAccess = nullptr) override;

    muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const override;

    void beginParameterGesture(EffectInstance* instance, const muse::String& parameterId, EffectSettingsAccessPtr settingsAccess) override;

    void endParameterGesture(EffectInstance* instance, const muse::String& parameterId) override;

    void onInstanceDestroyed(EffectInstance* instance) override;

    void beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess) override;

    void endParameterEditing(EffectInstance* instance) override;

    //! Clear cached parameters for a specific instance (call when instance is destroyed)
    void clearCache(EffectInstance* instance);

    //! Clear all cached parameters
    void clearAllCaches();

private:
    //! Parameter cache: instance -> (paramId -> ParamInfo)
    //! Stores AU3 ParamInfo objects with cached indices for O(1) lookups
    mutable std::unordered_map<EffectInstance*, std::unordered_map<uint32_t, VST3ParameterExtraction::ParamInfo> > m_paramCache;

    //! Gesture tracking: instance -> settingsAccess
    //! Stores the settings access pointer for the current gesture
    std::unordered_map<EffectInstance*, EffectSettingsAccessPtr> m_gestureSettings;
};
}
