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

    inline bool operator==(const Clip& k) const
    {
        return key == k.key &&
               title == k.title &&
               color == k.color &&
               groupId == k.groupId &&
               startTime == k.startTime &&
               endTime == k.endTime &&
               stereo == k.stereo &&
               pitch == k.pitch &&
               speed == k.speed &&
               optimizeForVoice == k.optimizeForVoice &
               stretchToMatchTempo == k.stretchToMatchTempo &&
               hasCustomColor == k.hasCustomColor;
    }

    inline bool operator!=(const Clip& k) const { return !this->operator==(k); }
};

using Clips = std::vector<Clip>;
}
