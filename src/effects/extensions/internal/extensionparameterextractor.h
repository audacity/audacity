/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"

namespace au::effects::extensions {
class ExtensionParameterExtractor final : public IParameterExtractorService
{
public:
    EffectFamily family() const override;
    ParameterInfoList extractParameters(EffectInstance* instance, EffectSettingsAccessPtr settingsAccess) const override;
    ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const override;
    double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const override;
    bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double value,
                           EffectSettingsAccessPtr settingsAccess) override;
    bool setParameterStringValue(EffectInstance* instance, const muse::String& parameterId, const muse::String& value,
                                 EffectSettingsAccessPtr settingsAccess) override;
    muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const override;
};
} // namespace au::effects::extensions
