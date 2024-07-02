#ifndef AU_PROСESSING_CLIP_H
#define AU_PROСESSING_CLIP_H

#include <vector>

#include "global/types/string.h"
#include "draw/types/color.h"
#include "../processingtypes.h"

namespace au::processing {
struct Clip {
    ClipKey key;
    muse::String title;
    muse::draw::Color color;
    double startTime = 0.0;
    double endTime = 0.0;
};

using Clips = std::vector<Clip>;
}

#endif // AU_PROСESSING_CLIP_H
