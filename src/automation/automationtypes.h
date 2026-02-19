/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedit/trackedittypes.h"

namespace au::automation {
struct AutomationPoint
{
    double xValue = 0.0;
    double yValue = 1.0;
};

struct AutomationInfo
{
    double minValue = 0.0;
    double maxValue = 1.0;
    double defaultValue = 1.0;
    bool exponential = false;
    std::uint64_t version = 0;
};

using AutomationPoints = std::vector<AutomationPoint>;

struct AutomationDragSession
{
    au::trackedit::ClipKey clip;
    int originalIndex = -1;
    double originalXValue = 0.0;
    double originalYValue = 0.0;
    bool active = false;

    // used for eat/restore behavior
    int currentDragIndex = -1;
    int lastConsumedLeft = -1;  // smallest removed index on the left side
    int lastConsumedRight = -1; // largest removed index on the right side
    AutomationPoints originalPoints;
};
}
