/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/effectstypes.h"
#include "au3-components/EffectInterface.h"

namespace au::effects {
constexpr auto toAu4EffectCategory(::EffectGroup group)
{
    using namespace au::effects;
    switch (group) {
    case ::EffectGroup::Unspecified: return BuiltinEffectCategoryId::Unspecified;
    case ::EffectGroup::None: return BuiltinEffectCategoryId::None;
    case ::EffectGroup::VolumeAndCompression: return BuiltinEffectCategoryId::VolumeAndCompression;
    case ::EffectGroup::Fading: return BuiltinEffectCategoryId::Fading;
    case ::EffectGroup::PitchAndTempo: return BuiltinEffectCategoryId::PitchAndTempo;
    case ::EffectGroup::EqAndFilters: return BuiltinEffectCategoryId::EqAndFilters;
    case ::EffectGroup::NoiseRemovalAndRepair: return BuiltinEffectCategoryId::NoiseRemovalAndRepair;
    case ::EffectGroup::DelayAndReverb: return BuiltinEffectCategoryId::DelayAndReverb;
    case ::EffectGroup::DistortionAndModulation: return BuiltinEffectCategoryId::DistortionAndModulation;
    case ::EffectGroup::Special: return BuiltinEffectCategoryId::Special;
    case ::EffectGroup::SpectralTools: return BuiltinEffectCategoryId::SpectralTools;
    case ::EffectGroup::Legacy: return BuiltinEffectCategoryId::Legacy;
    default:
        assert(false);
        return BuiltinEffectCategoryId::Unspecified;
    }
}
}
