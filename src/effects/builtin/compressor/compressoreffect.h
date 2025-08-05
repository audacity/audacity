/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-effects/PerTrackEffect.h"
#include "libraries/lib-components/SettingsVisitor.h"

namespace au::effects {
class CompressorEffect : public EffectWithSettings<CompressorSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    CompressorEffect();
    ~CompressorEffect() override = default;

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

    static constexpr EffectParameter thresholdDb {
        &CompressorSettings::thresholdDb,
        L"thresholdDb",
        compressorThresholdDbDefault,
        -60,
        0,
        0.1
    };

    static constexpr EffectParameter makeupGainDb {
        &CompressorSettings::makeupGainDb,
        L"makeupGainDb",
        compressorMakeupGainDbDefault,
        -30,
        30,
        0.1
    };

    static constexpr EffectParameter kneeWidthDb {
        &CompressorSettings::kneeWidthDb,
        L"kneeWidthDb",
        compressorKneeWidthDbDefault,
        0,
        30,
        0.1
    };

    static constexpr EffectParameter compressionRatio {
        &CompressorSettings::compressionRatio,
        L"compressionRatio",
        compressorCompressionRatioDefault,
        1,
        100,
        0.01
    };

    static constexpr EffectParameter lookaheadMs {
        &CompressorSettings::lookaheadMs, L"lookaheadMs",
        compressorLookaheadMsDefault,
        0,
        compressorMaxLookaheadMs,
        0.1
    };

    static constexpr EffectParameter attackMs {
        &CompressorSettings::attackMs,
        L"attackMs",
        compressorAttackMsDefault,
        0,
        200,
        0.1
    };

    static constexpr EffectParameter releaseMs {
        &CompressorSettings::releaseMs,
        L"releaseMs",
        compressorReleaseMsDefault,
        0,
        1000,
        0.1
    };

    static constexpr EffectParameter showInput {
        &CompressorSettings::showInput, L"showInput", showInputDefault, 0, 1, 1
    };
    static constexpr EffectParameter showOutput {
        &CompressorSettings::showOutput, L"showOutput", showOutputDefault, 0, 1, 1
    };
    static constexpr EffectParameter showActual {
        &CompressorSettings::showActual, L"showActual", showActualDefault, 0, 1, 1
    };
    static constexpr EffectParameter showTarget {
        &CompressorSettings::showTarget, L"showTarget", showTargetDefault, 0, 1, 1
    };
};
}
