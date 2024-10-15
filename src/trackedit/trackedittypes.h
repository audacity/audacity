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

using TrackId = long;
using ClipId = uint64_t;

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

struct TimeSignature
{
    double tempo = 0;

    int upper = 0;
    int lower = 0;
};
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::trackedit::ClipKey& k)
{
    s << "{trackId: " << k.trackId << ", clip: " << k.clipId << "}";
    return s;
}
