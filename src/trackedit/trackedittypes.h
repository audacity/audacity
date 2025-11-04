/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/types/number.h"
#include "framework/global/logstream.h"

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

using TrackItemId = int64_t;
using ClipId = TrackItemId;
using LabelId = TrackItemId;

using TrackIdList = std::vector<TrackId>;

struct TrackItemKey
{
    TrackId trackId = -1;
    TrackItemId itemId = -1;

    TrackItemKey() = default;
    TrackItemKey(const TrackId t, const TrackItemId o)
        : trackId(t), itemId(o) {}

    inline bool isValid() const { return trackId != -1 && itemId != static_cast<TrackItemId>(-1); }

    inline bool operator==(const TrackItemKey& k) const { return trackId == k.trackId && itemId == k.itemId; }
    inline bool operator!=(const TrackItemKey& k) const { return !this->operator==(k); }
    inline bool operator<(const TrackItemKey& k) const { return trackId < k.trackId || (trackId == k.trackId && itemId < k.itemId); }
};

using TrackItemKeyList = std::vector<TrackItemKey>;

using ClipKey = TrackItemKey;
using ClipKeyList = TrackItemKeyList;

using LabelKey = TrackItemKey;
using LabelKeyList = TrackItemKeyList;

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

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::trackedit::TrackItemKey& k)
{
    s << "{trackId: " << k.trackId << ", itemId: " << k.itemId << "}";
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
