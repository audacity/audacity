/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <cstdint>
#include <optional>
#include <variant>

namespace au::record {
struct TimedRecordingOptions {
    int64_t durationSeconds = 60 * 60;
    bool saveProject = false;
    bool exportAudio = false;
    bool quitAudacity = false;
};

struct TimedRecordingSchedule {
    using TimePoint = std::chrono::system_clock::time_point;
    using Duration = std::chrono::seconds;

    std::optional<TimePoint> start;
    std::variant<Duration, TimePoint> end = Duration::zero();

    TimePoint startTime(TimePoint now) const
    {
        return start && *start > now ? *start : now;
    }

    TimePoint endTime(TimePoint now) const
    {
        if (const TimePoint* endAt = std::get_if<TimePoint>(&end)) {
            return *endAt;
        }
        return startTime(now) + std::get<Duration>(end);
    }

    Duration recordingDuration(TimePoint now) const
    {
        return std::chrono::duration_cast<Duration>(endTime(now) - startTime(now));
    }

    bool isValid(TimePoint now) const
    {
        return recordingDuration(now) > Duration::zero();
    }
};
}
