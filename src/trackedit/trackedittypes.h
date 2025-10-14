/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/number.h"
#include "global/logstream.h"

namespace au::trackedit {
using msecs_t = int64_t;
using secs_t = muse::number_t<double>;
using samples_t = uint64_t;
using sample_rate_t = uint64_t;
using audioch_t = uint8_t;
using volume_db_t = float;
using volume_dbfs_t = float;
using gain_t = float;
using pan_t = float;

using TrackId = int64_t;
using LabelTrackId = int64_t;
constexpr TrackId INVALID_TRACK = -1;

using TrackObjectId = int64_t;
using ClipId = TrackObjectId;
using LabelId = TrackObjectId;

using TrackIdList = std::vector<TrackId>;

struct TrackObjectKey
{
    TrackId trackId = -1;
    TrackObjectId objectId = -1;

    TrackObjectKey() = default;
    TrackObjectKey(const TrackId t, const TrackObjectId o)
        : trackId(t), objectId(o) {}

    inline bool isValid() const { return trackId != -1 && objectId != static_cast<TrackObjectId>(-1); }

    inline bool operator==(const TrackObjectKey& k) const { return trackId == k.trackId && objectId == k.objectId; }
    inline bool operator!=(const TrackObjectKey& k) const { return !this->operator==(k); }
};

using TrackObjectKeyList = std::vector<TrackObjectKey>;

using ClipKey = TrackObjectKey;
using ClipKeyList = TrackObjectKeyList;

using LabelKey = TrackObjectKey;
using LabelKeyList = TrackObjectKeyList;

struct TimeSignature
{
    double tempo = 0;

    int upper = 0;
    int lower = 0;
};

enum class UndoPushType : unsigned char {
    NONE = 0,
    CONSOLIDATE = 1 << 0,
    NOAUTOSAVE = 1 << 1
};

enum class TrackMoveDirection {
    Up,
    Down,
    Top,
    Bottom
};

enum class DeleteBehavior {
    NotSet = -1,
    CloseGap,
    LeaveGap,
};

enum class CloseGapBehavior {
    ClipRipple,
    TrackRipple,
    AllTracksRipple,
};

enum class PasteBehavior {
    PasteOverlap,
    PasteInsert,
};

enum class PasteInsertBehavior {
    PasteInsert,
    PasteInsertRipple,
};
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::trackedit::TrackObjectKey& k)
{
    s << "{trackId: " << k.trackId << ", objectId: " << k.objectId << "}";
    return s;
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, au::trackedit::UndoPushType t)
{
    switch (t) {
    case au::trackedit::UndoPushType::NONE: s << "UndoPushType::NONE";
        break;
    case au::trackedit::UndoPushType::CONSOLIDATE: s << "UndoPushType::CONSOLIDATE";
        break;
    case au::trackedit::UndoPushType::NOAUTOSAVE: s << "UndoPushType::NOAUTOSAVE";
        break;
    }
    return s;
}
