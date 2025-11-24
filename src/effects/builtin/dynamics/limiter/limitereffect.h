/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "au3-effects/PerTrackEffect.h"
#include "au3-components/SettingsVisitor.h"
#include "dynamics/dynamicseffectsmacros.h"

namespace au::effects {
using LimiterParameter = EffectParameter<LimiterSettings, double, double, double>;

class LimiterEffect : public EffectWithSettings<LimiterSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    LimiterEffect();
    ~LimiterEffect() override = default;

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    ::EffectType GetType() const override;
    RealtimeSince RealtimeSupport() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings) const override;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;
    std::unique_ptr<EffectOutputs> MakeOutputs() const override;
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    const EffectParameterMethods& Parameters() const override;

#define LIMITER_PARAM(paramName, paramMin, paramDefault, paramMax, paramStep) \
    DYNAMICS_EFFECT_PARAM(LimiterSettings, paramName, paramMin, paramDefault, paramMax, paramStep)

    LIMITER_PARAM(thresholdDb, -30, limiterThresholdDbDefault, 0, 0.1);
    LIMITER_PARAM(makeupTargetDb, -30, limiterMakeupTargetDbDefault, 0, 0.1);
    LIMITER_PARAM(kneeWidthDb, 0, limiterKneeWidthDbDefault, 10, 0.1);
    LIMITER_PARAM(lookaheadMs, 0, limiterLookaheadMsDefault, limiterMaxLookaheadMs, 0.01);
    LIMITER_PARAM(releaseMs, 0, limiterReleaseMsDefault, 1000, 0.1);
    LIMITER_PARAM(showInput, 0, showInputDefault, 1, 1);
    LIMITER_PARAM(showOutput, 0, showOutputDefault, 1, 1);
    LIMITER_PARAM(showActual, 0, showActualDefault, 1, 1);
    LIMITER_PARAM(showTarget, 0, showTargetDefault, 1, 1);
};
}
