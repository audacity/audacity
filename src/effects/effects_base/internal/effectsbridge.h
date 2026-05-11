/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/audioplugins/audiopluginstypes.h"

#include "../effectstypes.h"

// Wire-format bridge between Audacity's typed EffectFamily and the
// muse_framework wire string stored in AudioResourceMeta::type / the JSON
// plugin cache. Each format's canonical wire string is owned by its
// implementing module (muse::vst::AUDIO_RESOURCE_TYPE_NAME, etc.); this
// bridge routes between the typed enum and the string identifier without
// exposing the per-format internal headers through effectstypes.h.

namespace au::effects {
std::string toWireString(EffectFamily family);
EffectFamily fromWireString(const std::string& s);
bool isResourceType(const muse::audioplugins::AudioResourceMeta& meta, EffectFamily family);
}
