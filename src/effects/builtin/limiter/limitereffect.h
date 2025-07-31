/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-effects/PerTrackEffect.h"
#include "libraries/lib-components/SettingsVisitor.h"

namespace au::effects {
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

    static constexpr auto dbStep = 0.1;

    static constexpr EffectParameter thresholdDb {
        &LimiterSettings::thresholdDb,
        L"thresholdDb",
        limiterThresholdDbDefault,
        -30 / dbStep,
        0,
        1 / dbStep
    };

    static constexpr EffectParameter makeupTargetDb {
        &LimiterSettings::makeupTargetDb,
        L"makeupTargetDb",
        limiterMakeupTargetDbDefault,
        -30 / dbStep,
        0,
        1 / dbStep
    };

    static constexpr EffectParameter kneeWidthDb {
        &LimiterSettings::kneeWidthDb,
        L"kneeWidthDb",
        limiterKneeWidthDbDefault,
        0,
        10 / dbStep,
        1 / dbStep
    };

    static constexpr EffectParameter lookaheadMs {
        &LimiterSettings::lookaheadMs, L"lookaheadMs",
        limiterLookaheadMsDefault,     0,
        limiterMaxLookaheadMs,         1
    };

    static constexpr EffectParameter releaseMs { &LimiterSettings::releaseMs,
                                                  L"releaseMs",
                                                  limiterReleaseMsDefault,
                                                  0,
                                                  1000,
                                                  1 };

    static constexpr EffectParameter showInput {
        &LimiterSettings::showInput, L"showInput", showInputDefault, 0, 1, 1
    };
    static constexpr EffectParameter showOutput {
        &LimiterSettings::showOutput, L"showOutput", showOutputDefault, 0, 1, 1
    };
    static constexpr EffectParameter showActual {
        &LimiterSettings::showActual, L"showActual", showActualDefault, 0, 1, 1
    };
    static constexpr EffectParameter showTarget {
        &LimiterSettings::showTarget, L"showTarget", showTargetDefault, 0, 1, 1
    };
};
}
