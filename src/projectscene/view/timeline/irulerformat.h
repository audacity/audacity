/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include "timelinecontext.h"

namespace au::projectscene {
enum class TickType {
    MAJOR,
    MINOR,
    MINORMINOR
};

struct IntervalInfo {
    double major = 0.0;         // distance between major ticks [s]
    double minor = 0.0;         // distance between minor ticks [s]
    double minorMinor = 0.0;    // distance between minorMinor ticks [s]
    int digits = 0;             // number of digits displayed after the decimal point [s]
};

class IRulerFormat
{
public:
    virtual ~IRulerFormat() = default;

    virtual IntervalInfo intervalInfo(TimelineContext* context) = 0;
    virtual QString label(double d, const IntervalInfo& timeIntervalInfo, TickType tickType, TimelineContext* context = nullptr) = 0;
};
}
