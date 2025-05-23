/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedittypes.h"

namespace au::trackedit {
class TimeSpan
{
public:
    TimeSpan(secs_t start, secs_t end);
    TimeSpan(double start, double end);

    secs_t start() const;
    secs_t end() const;
    secs_t duration() const;

    bool operator==(const TimeSpan& other) const;
    bool operator!=(const TimeSpan& other) const;

private:
    const secs_t m_start = 0;
    const secs_t m_end = 0;
};
}
