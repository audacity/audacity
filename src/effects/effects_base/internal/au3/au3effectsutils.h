/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/PluginDescriptor.h"
#include "effects/effects_base/effectstypes.h"

namespace au::effects {
constexpr auto toAu4EffectCategory(::EffectGroup group)
{
    using namespace au::effects;
    switch (group) {
    case ::EffectGroup::Unspecified: return EffectCategory::Unspecified;
    case ::EffectGroup::None: return EffectCategory::None;
    case ::EffectGroup::VolumeAndCompression: return EffectCategory::VolumeAndCompression;
    case ::EffectGroup::Fading: return EffectCategory::Fading;
    case ::EffectGroup::PitchAndTempo: return EffectCategory::PitchAndTempo;
    case ::EffectGroup::EqAndFilters: return EffectCategory::EqAndFilters;
    case ::EffectGroup::NoiseRemovalAndRepair: return EffectCategory::NoiseRemovalAndRepair;
    case ::EffectGroup::DelayAndReverb: return EffectCategory::DelayAndReverb;
    case ::EffectGroup::DistortionAndModulation: return EffectCategory::DistortionAndModulation;
    case ::EffectGroup::Special: return EffectCategory::Special;
    case ::EffectGroup::SpectralTools: return EffectCategory::SpectralTools;
    case ::EffectGroup::Legacy: return EffectCategory::Legacy;
    default:
        assert(false);
        return EffectCategory::Unspecified;
    }
}

constexpr ::EffectGroup toAu3EffectGroup(EffectCategory category)
{
    switch (category) {
    case EffectCategory::None: return ::EffectGroup::None;
    case EffectCategory::VolumeAndCompression: return ::EffectGroup::VolumeAndCompression;
    case EffectCategory::Fading: return ::EffectGroup::Fading;
    case EffectCategory::PitchAndTempo: return ::EffectGroup::PitchAndTempo;
    case EffectCategory::EqAndFilters: return ::EffectGroup::EqAndFilters;
    case EffectCategory::NoiseRemovalAndRepair: return ::EffectGroup::NoiseRemovalAndRepair;
    case EffectCategory::DelayAndReverb: return ::EffectGroup::DelayAndReverb;
    case EffectCategory::DistortionAndModulation: return ::EffectGroup::DistortionAndModulation;
    case EffectCategory::Special: return ::EffectGroup::Special;
    case EffectCategory::SpectralTools: return ::EffectGroup::SpectralTools;
    case EffectCategory::Legacy: return ::EffectGroup::Legacy;
    case EffectCategory::Unspecified: return ::EffectGroup::Unspecified;
    default: return ::EffectGroup::Unspecified;
    }
}

constexpr auto toAu4EffectType(::EffectType type)
{
    using namespace au::effects;
    switch (type) {
    case ::EffectTypeNone: return EffectType::Unknown;
    case ::EffectTypeGenerate: return EffectType::Generator;
    case ::EffectTypeProcess: return EffectType::Processor;
    case ::EffectTypeAnalyze: return EffectType::Analyzer;
    case ::EffectTypeTool: return EffectType::Tool;
    default:
        assert(false);
        return EffectType::Unknown;
    }
}

constexpr ::EffectType toAu3EffectType(EffectType type)
{
    switch (type) {
    case EffectType::Generator: return ::EffectTypeGenerate;
    case EffectType::Processor: return ::EffectTypeProcess;
    case EffectType::Analyzer: return ::EffectTypeAnalyze;
    case EffectType::Tool: return ::EffectTypeTool;
    default: return ::EffectTypeNone;
    }
}

EffectMeta toEffectMeta(const ::PluginDescriptor& desc);
}
