/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/secs.h" // IWYU pragma: export
#include "au3audio/audiotypes.h"

namespace au::playback {
using msecs_t = int64_t; //! TODO need to remove

using TrackId = int32_t;

using TrackSequenceId = int32_t;
using TrackSequenceIdList = std::vector<TrackSequenceId>;

enum class PlaybackStatus {
    Stopped = 0,
    Paused,
    Running
};

struct PlaybackRegion
{
    muse::secs_t start;
    muse::secs_t end;

    inline bool isValid() const { return start != end; }

    inline bool operator==(const PlaybackRegion& other) const { return start == other.start && end == other.end; }
    inline bool operator!=(const PlaybackRegion& other) const { return !this->operator==(other); }
};

static constexpr audio::volume_dbfs_t MAX_DISPLAYED_DBFS = 0.f; // 100%
static constexpr audio::volume_dbfs_t MIN_DISPLAYED_DBFS = -60.f; // 0%

struct PlayTracksOptions {
    bool selectedOnly = false;
    double mixerEndTime = -1.0;  // Time at which mixer stops producing, maybe > endTime, if not set then == endTime
    double startOffset = 0.0;
    bool isDefaultPolicy = true;
};
}
