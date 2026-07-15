/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string_view>

#include "effects/effects_base/effectstypes.h"

namespace au::audacityplugin {
struct EffectDescriptor;
}

namespace au::effects::audacity_plugin {
inline constexpr std::string_view AUDIO_RESOURCE_TYPE_NAME = "audacity-plugin";
inline constexpr const char* EFFECT_FAMILY_ID = "AudacityPlugin";

EffectId makeEffectId(const au::audacityplugin::EffectDescriptor& descriptor);
} // namespace au::effects::audacity_plugin
