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
using balance_t = float;

using TrackId = int64_t;
constexpr TrackId INVALID_TRACK = -1;

using ClipId = int64_t;

using TrackIdList = std::vector<TrackId>;

struct ClipKey
{
    TrackId trackId = -1;
    ClipId clipId = -1;

    ClipKey() = default;
    ClipKey(const TrackId t, const ClipId c)
        : trackId(t), clipId(c) {}

    inline bool isValid() const { return trackId != -1 && clipId != static_cast<ClipId>(-1); }

    inline bool operator==(const ClipKey& k) const { return trackId == k.trackId && clipId == k.clipId; }
    inline bool operator!=(const ClipKey& k) const { return !this->operator==(k); }
};

using ClipKeyList = std::vector<ClipKey>;

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
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::trackedit::ClipKey& k)
{
    s << "{trackId: " << k.trackId << ", clip: " << k.clipId << "}";
    return s;
}
