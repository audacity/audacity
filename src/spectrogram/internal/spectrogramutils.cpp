/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramutils.h"

#include <cmath>

namespace au::spectrogram {
ZoomInfo::ZoomInfo(double zoom, double viewportT0, double viewportT1)
    : zoom{zoom}, viewportT0{viewportT0}, viewportT1{viewportT1}
{
}

int ZoomInfo::viewportWidth() const
{
    return static_cast<int>(std::round((viewportT1 - viewportT0) * zoom));
}

double ZoomInfo::positionToTime(int64_t position) const
{
    return viewportT0 + position / zoom;
}

int64_t ZoomInfo::timeToPosition(double projectTime) const
{
    double t = 0.5 + zoom * (projectTime - viewportT0);
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
