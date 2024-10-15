/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-wave-track/WaveClip.h"

namespace au::au3 {
struct WaveClipID {
    uint64_t id = -1;

    WaveClipID(const WaveClip* au3clip)
        : id(reinterpret_cast<uint64_t>(au3clip)) {}
};
}
