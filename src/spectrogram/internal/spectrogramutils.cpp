/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramutils.h"

#include <cmath>

namespace au::spectrogram {
int viewportWidth(const ViewInfo& viewInfo)
{
    return static_cast<int>(std::round((viewInfo.viewportT1 - viewInfo.viewportT0) * viewInfo.pixelsPerSecond));
}

double positionToTime(const ViewInfo& viewInfo, int position)
{
    return viewInfo.viewportT0 + position / viewInfo.pixelsPerSecond;
}

int timeToPosition(const ViewInfo& viewInfo, double projectTime)
{
    double t = 0.5 + viewInfo.pixelsPerSecond * (projectTime - viewInfo.viewportT0);
    if (t < INT64_MIN) {
        return INT64_MIN;
    }
    if (t > INT64_MAX) {
        return INT64_MAX;
    }
    t = std::floor(t);
    return t;
}
}
