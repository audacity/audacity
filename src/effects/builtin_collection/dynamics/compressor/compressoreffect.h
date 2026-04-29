/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "au3-effects/PerTrackEffect.h"
#include "au3-components/SettingsVisitor.h"
#include "dynamics/dynamicseffectsmacros.h"

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
    EffectGroup GetGroup() const override { return EffectGroup::VolumeAndCompression; }
    RealtimeSince RealtimeSupport() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings) const override;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;
    std::unique_ptr<EffectOutputs> MakeOutputs() const override;
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    const EffectParameterMethods& Parameters() const override;

#define COMPRESSOR_PARAM(paramName, paramMin, paramDefault, paramMax, paramStep) \
    DYNAMICS_EFFECT_PARAM(CompressorSettings, paramName, paramMin, paramDefault, paramMax, paramStep)

    COMPRESSOR_PARAM(thresholdDb, -60, compressorThresholdDbDefault, 0, 0.1);
    COMPRESSOR_PARAM(makeupGainDb, 0, compressorMakeupGainDbDefault, 30, 0.1);
    COMPRESSOR_PARAM(kneeWidthDb, 0, compressorKneeWidthDbDefault, 30, 0.1);
    COMPRESSOR_PARAM(compressionRatio, 1, compressorCompressionRatioDefault, 100, 0.01);
    COMPRESSOR_PARAM(lookaheadMs, 0, compressorLookaheadMsDefault, compressorMaxLookaheadMs, 0.1);
    COMPRESSOR_PARAM(attackMs, 0.0, compressorAttackMsDefault, 200, 0.01);
    COMPRESSOR_PARAM(releaseMs, 0.1, compressorReleaseMsDefault, 1000, 0.1);
    COMPRESSOR_PARAM(showInput, 0, showInputDefault, 1, 1);
    COMPRESSOR_PARAM(showOutput, 0, showOutputDefault, 1, 1);
    COMPRESSOR_PARAM(showActual, 0, showActualDefault, 1, 1);
    COMPRESSOR_PARAM(showTarget, 0, showTargetDefault, 1, 1);
};
}
