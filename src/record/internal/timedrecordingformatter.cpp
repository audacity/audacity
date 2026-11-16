/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingformatter.h"

#include <algorithm>

#include "framework/global/translation.h"

using namespace au::record;

namespace {
constexpr int64_t SECONDS_PER_MINUTE = 60;
constexpr int64_t SECONDS_PER_HOUR = 60 * SECONDS_PER_MINUTE;
constexpr int64_t SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;
}

muse::String TimedRecordingFormatter::duration(std::chrono::seconds value)
{
    const int64_t seconds = std::max<int64_t>(value.count(), 0);

    const int64_t days = seconds / SECONDS_PER_DAY;
    const int64_t hours = seconds % SECONDS_PER_DAY / SECONDS_PER_HOUR;
    const int64_t minutes = seconds % SECONDS_PER_HOUR / SECONDS_PER_MINUTE;
    const int64_t secs = seconds % SECONDS_PER_MINUTE;

    muse::StringList parts;
    if (days > 0) {
        //: Abbreviated number of days in a duration, e.g. "2d"
        parts.push_back(muse::mtrc("record", "%1d").arg(days));
    }
    if (hours > 0) {
        //: Abbreviated number of hours in a duration, e.g. "1h"
        parts.push_back(muse::mtrc("record", "%1h").arg(hours));
    }
    if (minutes > 0) {
        //: Abbreviated number of minutes in a duration, e.g. "30m"
        parts.push_back(muse::mtrc("record", "%1m").arg(minutes));
    }
    if (secs > 0 || parts.empty()) {
        //: Abbreviated number of seconds in a duration, e.g. "45s"
        parts.push_back(muse::mtrc("record", "%1s").arg(secs));
    }

    return parts.join(u" ");
}
