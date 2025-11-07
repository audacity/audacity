/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramtypes.h"

#include <cmath>

namespace au::projectscene {
ZoomInfo::ZoomInfo(double zoom, double viewportTime)
    : zoom{zoom}, viewportTime{viewportTime}
{
}

double ZoomInfo::PositionToTime(int64_t position, int64_t origin) const
{
    return viewportTime + (position - origin) / zoom;
}

int64_t ZoomInfo::TimeToPosition(double projectTime, int64_t origin) const
{
    double t = 0.5 + zoom * (projectTime - viewportTime) + origin;
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
