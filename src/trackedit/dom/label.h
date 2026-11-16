/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "../trackedittypes.h"

namespace au::trackedit {
struct Label {
    LabelKey key;

    muse::String title;
    ClipColorIndex colorIndex = CLIP_COLOR_INDEX_NONE;
    double startTime = 0.0;
    double endTime = 0.0;
    double lowFrequency = 0.0;
    double highFrequency = 0.0;
    int64_t groupId = -1;

    inline bool isValid() const { return key.isValid(); }
};

using Labels = std::vector<Label>;
}
