/**********************************************************************

  Audacity: A Digital Audio Editor

  ZoomInfo.cpp

  Paul Licameli split from ViewInfo.cpp

**********************************************************************/

#include "ZoomInfo.h"

#include <cassert>
#include <cmath>

namespace {
static const double gMaxZoom = 6000000;
static const double gMinZoom = 0.001;
}

ZoomInfo::ZoomInfo(double start, double pixelsPerSecond)
    : hpos{start}
    , zoom{pixelsPerSecond}
{
}

ZoomInfo::~ZoomInfo()
{
}

/// Converts a position (mouse X coordinate) to
/// project time, in seconds.  Needs the left edge of
/// the track as an additional parameter.
double ZoomInfo::PositionToTime(int64 position,
                                int64 origin,
                                bool // ignoreFisheye
                                ) const
{
    return hpos + (position - origin) / zoom;
}

/// STM: Converts a project time to screen x position.
auto ZoomInfo::TimeToPosition(double projectTime,
                              int64 origin,
                              bool // ignoreFisheye
                              ) const -> int64
{
    double t = 0.5 + zoom * (projectTime - hpos) + origin;
    if (t < INT64_MIN) {
        return INT64_MIN;
    }
    if (t > INT64_MAX) {
        return INT64_MAX;
    }
    t = floor(t);
    return t;
}

// This always ignores the fisheye.  Use with caution!
// You should prefer to call TimeToPosition twice, for endpoints, and take the difference!
double ZoomInfo::TimeRangeToPixelWidth(double timeRange) const
{
    return timeRange * zoom;
}

bool ZoomInfo::ZoomInAvailable() const
{
    return zoom < gMaxZoom;
}

bool ZoomInfo::ZoomOutAvailable() const
{
    return zoom > gMinZoom;
}

double ZoomInfo::GetZoom() const { return zoom; }

double ZoomInfo::GetAbsoluteOffset(double offset) const
{
    return std::floor(0.5 + hpos * zoom + offset);
}

double ZoomInfo::GetMaxZoom()
{
    return gMaxZoom;
}

double ZoomInfo::GetMinZoom() { return gMinZoom; }

void ZoomInfo::SetZoom(double pixelsPerSecond)
{
    zoom = std::max(gMinZoom, std::min(gMaxZoom, pixelsPerSecond));
}

void ZoomInfo::ZoomBy(double multiplier)
{
    SetZoom(zoom * multiplier);
}

ZoomInfo::Intervals
ZoomInfo::FindIntervals(int64 width, int64 origin) const
{
    ZoomInfo::Intervals results;
    results.reserve(2);

    const int64 rightmost(origin + (0.5 + width));
    assert(origin <= rightmost);
    {
        results.push_back(Interval(origin, zoom, false));
    }

    if (origin < rightmost) {
        results.push_back(Interval(rightmost, 0, false));
    }
    assert(!results.empty() && results[0].position == origin);
    return results;
}
