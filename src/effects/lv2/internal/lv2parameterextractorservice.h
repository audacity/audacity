/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/iparameterextractorservice.h"
#include "effects/effects_base/ieffectinstancesregister.h"

#include "framework/global/modularity/ioc.h"

namespace au::effects {
//! Implementation of parameter extraction for LV2 plugins.
//! Parameters are the plugin's control ports; values live in
//! LV2EffectSettings::values (one entry per control port, addressed here by
//! the port symbol, which the LV2 spec guarantees to be unique per plugin).
class Lv2ParameterExtractorService : public IParameterExtractorService
{
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

public:
    EffectFamily family() const override { return EffectFamily::LV2; }

    ParameterInfoList extractParameters(EffectInstance* instance) const override;

    ParameterInfo getParameter(EffectInstance* instance, const muse::String& parameterId) const override;

    double getParameterValue(EffectInstance* instance, const muse::String& parameterId) const override;

    bool setParameterValue(EffectInstance* instance, const muse::String& parameterId, double fullRangeValue) override;

    muse::String getParameterValueString(EffectInstance* instance, const muse::String& parameterId, double value) const override;
};
} // namespace au::effects
