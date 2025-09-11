/*
 * Audacity: A Digital Audio Editor
 */
#include "limitereffect.h"

#include "libraries/lib-command-parameters/ShuttleAutomation.h"
#include "libraries/lib-builtin-effects/CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorDummyOutputs.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorUtils.h"

namespace au::effects {
const ComponentInterfaceSymbol LimiterEffect::Symbol { XO("Limiter") };

const EffectParameterMethods& LimiterEffect::Parameters() const
{
    static CapturedParameters<LimiterEffect, thresholdDb, makeupTargetDb, kneeWidthDb, lookaheadMs, releaseMs, showInput, showOutput,
                              showActual, showTarget> parameters;
    return parameters;
}

std::shared_ptr<EffectInstance> LimiterEffect::MakeInstance() const
{
    return std::make_shared<CompressorInstance>(*this);
}

std::unique_ptr<EffectOutputs> LimiterEffect::MakeOutputs() const
{
    return std::make_unique<DynamicRangeProcessorDummyOutputs>();
}

bool LimiterEffect::CheckWhetherSkipEffect(const EffectSettings&) const
{
    // Given the infinite ratio, a limiter is always susceptible to modifying the
    // audio.
    return false;
}

LimiterEffect::LimiterEffect()
{
    SetLinearEffectFlag(false);
}

ComponentInterfaceSymbol LimiterEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString LimiterEffect::GetDescription() const
{
    return XO("Augments loudness while minimizing distortion.");
}

ManualPageID LimiterEffect::ManualPage() const
{
    return L"";
}

EffectType LimiterEffect::GetType() const
{
    return EffectTypeProcess;
}

auto LimiterEffect::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::Always;
}

RegistryPaths LimiterEffect::GetFactoryPresets() const
{
    const auto presets = DynamicRangeProcessorUtils::GetLimiterPresets();
    RegistryPaths paths(presets.size());
    std::transform(
        presets.begin(), presets.end(), paths.begin(), [](const auto& preset) {
        return RegistryPath { preset.name.Translation() };
    });
    return paths;
}

OptionalMessage
LimiterEffect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    const auto presets = DynamicRangeProcessorUtils::GetLimiterPresets();
    if (id < 0 || id >= presets.size()) {
        return {};
    }
    LimiterEffect::GetSettings(settings) = presets[id].preset;
    return { nullptr };
}
}
