/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"

#include <map>

namespace au::effects {
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

    void endParameterGesture(EffectInstance* instance, const muse::String& parameterId) override;

    void onInstanceDestroyed(EffectInstance* instance) override;

    void beginParameterEditing(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess) override;

    void endParameterEditing(EffectInstance* instance) override;

private:
    EffectSettingsAccessPtr sessionSettings(EffectInstance* instance) const;

    std::map<EffectInstance*, EffectSettingsAccessPtr> m_sessionSettings;
};
}
