/*
 * Audacity: A Digital Audio Editor
 */
#include "compressoreffect.h"

#include "libraries/lib-command-parameters/ShuttleAutomation.h"
#include "libraries/lib-builtin-effects/CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorDummyOutputs.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorUtils.h"

#include "global/types/number.h"

namespace au::effects {
const ComponentInterfaceSymbol CompressorEffect::Symbol { XO("Compressor") };

const EffectParameterMethods& CompressorEffect::Parameters() const
{
    static CapturedParameters<
        CompressorEffect, thresholdDb, makeupGainDb, kneeWidthDb, compressionRatio, lookaheadMs, attackMs, releaseMs, showInput, showOutput,
        showActual, showTarget> parameters;
    return parameters;
}

std::shared_ptr<EffectInstance> CompressorEffect::MakeInstance() const
{
    return std::make_shared<CompressorInstance>(*this);
}

std::unique_ptr<EffectOutputs> CompressorEffect::MakeOutputs() const
{
    return std::make_unique<DynamicRangeProcessorDummyOutputs>();
}

bool CompressorEffect::CheckWhetherSkipEffect(
    const EffectSettings& settings) const
{
    const auto& compressorSettings = GetSettings(settings);
    return muse::is_equal(compressorSettings.compressionRatio, 1.0)
           && muse::is_equal(compressorSettings.makeupGainDb, 0.0)
           && // Also look-ahead, as this adds delay when used on a real-time input
              // (mic).
           muse::is_equal(compressorSettings.lookaheadMs, 0.0);
}

CompressorEffect::CompressorEffect()
{
    SetLinearEffectFlag(false);
}

ComponentInterfaceSymbol CompressorEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString CompressorEffect::GetDescription() const
{
    return XO(
        "Reduces \"dynamic range\", or differences between loud and quiet parts.");
}

ManualPageID CompressorEffect::ManualPage() const
{
    return L"";
}

EffectType CompressorEffect::GetType() const
{
    return EffectTypeProcess;
}

auto CompressorEffect::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::Always;
}

RegistryPaths CompressorEffect::GetFactoryPresets() const
{
    const auto presets = DynamicRangeProcessorUtils::GetCompressorPresets();
    RegistryPaths paths(presets.size());
    std::transform(
        presets.begin(), presets.end(), paths.begin(), [](const auto& preset) {
        return RegistryPath { preset.name.Translation() };
    });
    return paths;
}

OptionalMessage
CompressorEffect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    const auto presets = DynamicRangeProcessorUtils::GetCompressorPresets();
    if (id < 0 || id >= presets.size()) {
        return {};
    }
    CompressorEffect::GetSettings(settings) = presets[id].preset;
    return { nullptr };
}
}
