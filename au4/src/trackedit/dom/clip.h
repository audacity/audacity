/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "draw/types/color.h"
#include "../trackedittypes.h"

namespace au::trackedit {
struct Clip {
    ClipKey key;
    muse::String title;
    muse::draw::Color color;
    double startTime = 0.0;
    double endTime = 0.0;
    bool stereo = false;

    inline bool isValid() const { return key.isValid(); }
};

using Clips = std::vector<Clip>;
}
