/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>

namespace au::projectscene {
struct ZoomInfo {
    ZoomInfo(double zoom, double viewportT0, double viewportT1);

    const double zoom;
    const double viewportT0;
    const double viewportT1;

    int viewportWidth() const;

    double PositionToTime(int64_t position, int64_t origin = 0) const;
    int64_t TimeToPosition(double projectTime, int64_t origin = 0) const;
};

struct SelectedRegion {
    static constexpr int UndefinedFrequency = -1;

    SelectedRegion(double t0 = 0.0, double t1 = 0.0, double f0 = UndefinedFrequency, double f1 = UndefinedFrequency)
        : t0{t0}, t1{t1}, f0{f0}, f1{f1}
    {}

    const double t0;
    const double t1;
    const double f0;
    const double f1;
};
}
