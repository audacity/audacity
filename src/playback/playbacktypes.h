/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "actions/actiontypes.h"
#include "au3audio/audiotypes.h"
#include "global/types/secs.h" // IWYU pragma: export

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

enum class TimecodeFormatType {
    Undefined = -1,
    Seconds,
    SecondsMilliseconds,
    HHMMSS,
    DDHHMMSS,
    HHMMSSHundredths,
    HHMMSSMilliseconds,
    HHMMSSSamples,
    Samples,
    HHMMSSFilmFrames,
    FilmFrames,
    HHMMSSNTSCDropFrames,
    HHMMSSNTSCNonDropFrames,
    NTSCFrames,
    HHMMSSPALFrames,
    PALFrames,
    HHMMSSCDDAFrames,
    CDDAFrames,
    BarBeat,
    BarBeatTick
};

class TracksBehaviors
{
    Q_GADGET
public:
    enum class SoloBehavior {
        SoloBehaviorSimple = 0,
        SoloBehaviorMulti
    };
    Q_ENUM(SoloBehavior)
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

class DitherTypePrefs
{
    Q_GADGET
public:
    enum class DitherType {
        None = 0,
        Rectangle = 1,
        Triangle = 2,
        Shaped = 3
    };
    Q_ENUM(DitherType)
};

class PlaybackQualityPrefs
{
    Q_GADGET
public:
    enum class PlaybackQuality {
        LowQuality = 0,
        MediumQuality = 1,
        HighQuality = 2,
        BestQuality = 3
    };
    Q_ENUM(PlaybackQuality)
};

class PlaybackMeterType
{
    Q_GADGET
public:
    enum class MeterType {
        DbLog = 0,
        DbLinear = 1,
        Linear = 2,
    };
    Q_ENUM(MeterType)
};

class PlaybackMeterStyle
{
    Q_GADGET
public:
    enum class MeterStyle {
        Default,
        RMS,
        Gradient,
    };
    Q_ENUM(MeterStyle)
};

class PlaybackMeterPosition
{
    Q_GADGET
public:
    enum class MeterPosition {
        TopBar = 0,
        SideBar = 1,
    };
    Q_ENUM(MeterPosition)
};

class PlaybackMeterDbRange
{
    Q_GADGET
public:
    enum class DbRange: int {
        Range36 = 0,
        Range48 = 1,
        Range60 = 2,
        Range72 = 3,
        Range84 = 4,
        Range96 = 5,
        Range120 = 6,
        Range145 = 7,
    };
    Q_ENUM(DbRange)
};
}
