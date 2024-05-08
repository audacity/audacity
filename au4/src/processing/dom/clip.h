#ifndef AU_PROСESSING_CLIP_H
#define AU_PROСESSING_CLIP_H

#include <vector>

#include "global/types/string.h"

namespace au::processing {
struct Clip {
    muse::String title;
    double startTime = 0.0;
    double endTime = 0.0;

    //! NOTE Temporary
    uintptr_t au3WaveTrackPtr = 0; // WaveTrack*
    uintptr_t au3WaveClipPtr = 0; // WaveClip*
};

using Clips = std::vector<Clip>;
}

#endif // AU_PROСESSING_CLIP_H
