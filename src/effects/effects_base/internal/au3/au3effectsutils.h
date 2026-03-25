/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/PluginDescriptor.h"
#include "effects/effects_base/effectstypes.h"
#include "au3-components/EffectInterface.h"

class PluginDescriptor;

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

EffectMeta toEffectMeta(const ::PluginDescriptor& desc, EffectFamily, const muse::String& title, const muse::String& description,
                        bool supportsMultipleClipSelection);
}
