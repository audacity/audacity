/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Limiter.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "Limiter.h"
#include "CompressorInstance.h" // A limiter is just a compressor with hard-coded parameters.
#include "DynamicRangeProcessorDummyOutputs.h"
#include "DynamicRangeProcessorEffectUtils.h"
#include "DynamicRangeProcessorUtils.h"
#include "LimiterEditor.h"
#include "LoadEffects.h"
#include "ShuttleAutomation.h"

const ComponentInterfaceSymbol EffectLimiter::Symbol { XO("Limiter") };

namespace {
BuiltinEffectsModule::Registration<EffectLimiter> reg;
} // namespace

const EffectParameterMethods& EffectLimiter::Parameters() const
{
    static CapturedParameters<
        EffectLimiter, LimiterEditor::thresholdDb, LimiterEditor::makeupTargetDb,
        LimiterEditor::kneeWidthDb, LimiterEditor::lookaheadMs,
        LimiterEditor::releaseMs, LimiterEditor::showInput,
        LimiterEditor::showOutput, LimiterEditor::showActual,
        LimiterEditor::showTarget>
    parameters;
    return parameters;
}

std::shared_ptr<EffectInstance> EffectLimiter::MakeInstance() const
{
    return std::make_shared<CompressorInstance>(*this);
}

bool EffectLimiter::CheckWhetherSkipEffect(const EffectSettings& settings) const
{
    // Given the infinite ratio, a limiter is always susceptible to modifying the
    // audio.
    return false;
}

EffectLimiter::EffectLimiter()
{
    SetLinearEffectFlag(false);
}

ComponentInterfaceSymbol EffectLimiter::GetSymbol() const
{
    return Symbol;
}

TranslatableString EffectLimiter::GetDescription() const
{
    return XO("Augments loudness while minimizing distortion.");
}

ManualPageID EffectLimiter::ManualPage() const
{
    // TODO
    return L"";
}

EffectType EffectLimiter::GetType() const
{
    return EffectTypeProcess;
}

auto EffectLimiter::RealtimeSupport() const -> RealtimeSince
{
    return RealtimeSince::Always;
}

RegistryPaths EffectLimiter::GetFactoryPresets() const
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
EffectLimiter::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    const auto presets = DynamicRangeProcessorUtils::GetLimiterPresets();
    if (id < 0 || id >= presets.size()) {
        return {}
    }
    EffectLimiter::GetSettings(settings) = presets[id].preset;
    return { nullptr };
}

std::unique_ptr<EffectEditor> EffectLimiter::MakeEditor(
    ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
    const EffectOutputs* pOutputs) const
{
    return DynamicRangeProcessorEffectUtils::MakeLimiterEditor(
        S, instance, access, *this, pOutputs, GetSettings(access.Get()));
}

std::unique_ptr<EffectOutputs> EffectLimiter::MakeOutputs() const
{
    return std::make_unique<DynamicRangeProcessorDummyOutputs>();
}
