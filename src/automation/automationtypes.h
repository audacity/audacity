/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedit/trackedittypes.h"

namespace au::automation {
struct ClipEnvelopePoint
{
    double time = 0.0;
    double value = 1.0;
};

struct ClipEnvelopeInfo
{
    double minValue = 0.0;
    double maxValue = 1.0;
    double defaultValue = 1.0;
    bool exponential = false;
    std::uint64_t version = 0;
};

using ClipEnvelopePoints = std::vector<ClipEnvelopePoint>;

struct EnvelopeDragSession
{
    au::trackedit::ClipKey clip;
    int origIndex = -1;
    double origTime = 0.0;
    double origValue = 0.0;
    bool active = false;

    // used for eat/restore behavior
    int currentDragIndex = -1;
    int lastConsumedLeft = -1;  // smallest removed index on the left side
    int lastConsumedRight = -1; // largest removed index on the right side
    ClipEnvelopePoints originalPoints;
};
}
