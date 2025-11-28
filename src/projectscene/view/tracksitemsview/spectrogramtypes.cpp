/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramtypes.h"

#include <cmath>

namespace au::projectscene {
ZoomInfo::ZoomInfo(double zoom, double viewportT0, double viewportT1)
    : zoom{zoom}, viewportT0{viewportT0}, viewportT1{viewportT1}
{
}

int ZoomInfo::viewportWidth() const
{
    return static_cast<int>(std::round((viewportT1 - viewportT0) * zoom));
}

double ZoomInfo::PositionToTime(int64_t position, int64_t origin) const
{
    return viewportT0 + (position - origin) / zoom;
}

int64_t ZoomInfo::TimeToPosition(double projectTime, int64_t origin) const
{
    double t = 0.5 + zoom * (projectTime - viewportT0) + origin;
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
