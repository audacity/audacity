/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <cmath>

namespace au::spectrogram {
struct ZoomInfo {
    ZoomInfo(double zoom, double viewportT0, double viewportT1);

    const double zoom;
    const double viewportT0;
    const double viewportT1;

    int viewportWidth() const;

    double positionToTime(int64_t position) const;
    int64_t timeToPosition(double projectTime) const;
};
}
