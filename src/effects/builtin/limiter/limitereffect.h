/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-effects/PerTrackEffect.h"
#include "libraries/lib-components/SettingsVisitor.h"

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

    static constexpr LimiterParameter thresholdDb {
        &LimiterSettings::thresholdDb,
        L"thresholdDb",
        limiterThresholdDbDefault,
        -30,
        0,
        0.1
    };

    static constexpr LimiterParameter makeupTargetDb {
        &LimiterSettings::makeupTargetDb,
        L"makeupTargetDb",
        limiterMakeupTargetDbDefault,
        -30,
        0,
        0.1
    };

    static constexpr LimiterParameter kneeWidthDb {
        &LimiterSettings::kneeWidthDb,
        L"kneeWidthDb",
        limiterKneeWidthDbDefault,
        0,
        10,
        0.1
    };

    static constexpr LimiterParameter lookaheadMs {
        &LimiterSettings::lookaheadMs, L"lookaheadMs",
        limiterLookaheadMsDefault,
        0,
        limiterMaxLookaheadMs,
        0.1
    };

    static constexpr LimiterParameter releaseMs { &LimiterSettings::releaseMs,
                                                  L"releaseMs",
                                                  limiterReleaseMsDefault,
                                                  0,
                                                  1000,
                                                  0.1 };

    static constexpr LimiterParameter showInput {
        &LimiterSettings::showInput, L"showInput", showInputDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showOutput {
        &LimiterSettings::showOutput, L"showOutput", showOutputDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showActual {
        &LimiterSettings::showActual, L"showActual", showActualDefault, 0, 1, 1
    };
    static constexpr LimiterParameter showTarget {
        &LimiterSettings::showTarget, L"showTarget", showTargetDefault, 0, 1, 1
    };
};
}
