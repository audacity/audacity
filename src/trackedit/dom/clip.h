/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "draw/types/color.h"
#include "../trackedittypes.h"

namespace au::trackedit {
using ClipVersion = int64_t;

struct Clip {
    ClipKey key;
    ClipVersion clipVersion = -1;

    muse::String title;
    muse::draw::Color color;
    int groupId = -1;
    double startTime = 0.0;
    double endTime = 0.0;
    bool stereo = false;

    int pitch = 0;
    double speed = 0.0;
    bool optimizeForVoice = false;

    bool stretchToMatchTempo = false;
    bool hasCustomColor = false;

    inline bool isValid() const { return key.isValid(); }
};

using Clips = std::vector<Clip>;
}
