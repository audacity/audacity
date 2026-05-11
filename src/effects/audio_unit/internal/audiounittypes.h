/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string_view>

namespace au::effects::audio_unit {
// Canonical wire-format identifier for AudioUnit plugin entries in the
// muse_framework plugin cache. Owned by this module; bridged to the typed
// EffectFamily enum by au::effects::toWireString / fromWireString.
inline constexpr std::string_view AUDIO_RESOURCE_TYPE_NAME = "AudioUnit";
}
