/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"

#include <unordered_map>

namespace au::effects {
//! Implementation of parameter extraction for LV2 plugins.
//! Parameters are the plugin's control ports; values live in
//! LV2EffectSettings::values (one entry per control port, addressed here by
//! the port symbol, which the LV2 spec guarantees to be unique per plugin).
class Lv2ParameterExtractorService : public IParameterExtractorService
{
public:
    EffectFamily family() const override { return EffectFamily::LV2; }

    ParameterInfoList extractParameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess = nullptr) const override;

    ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const override;

    double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const override;

    bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double fullRangeValue,
                           EffectSettingsAccessPtr settingsAccess = nullptr) override;

    muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const override;

    void beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess) override;

    void endParameterEditing(EffectInstance* instance) override;

    void onInstanceDestroyed(EffectInstance* instance) override;

private:
    //! getParameter/getParameterValue don't receive a settings access, so
    //! remember the one of the editing session to read current values back.
    //! (Mutable because the const extraction methods also refresh it.)
    mutable std::unordered_map<EffectInstance*, EffectSettingsAccessPtr> m_settingsAccess;
};
}
