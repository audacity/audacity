/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/containers.h"
#include "global/types/number.h"
#include "global/logstream.h"

namespace au::processing {
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

struct ClipKey
{
    TrackId trackId = -1;
    size_t index = muse::nidx;

    ClipKey() = default;
    ClipKey(const TrackId t, const size_t i)
        : trackId(t), index(i) {}

    inline bool operator==(const ClipKey& k) const { return trackId == k.trackId && index == k.index; }
    inline bool operator!=(const ClipKey& k) const { return !this->operator==(k); }
};

struct TimeSignature
{
    double tempo = 0;

    int upper = 0;
    int lower = 0;
};
}

inline muse::logger::Stream& operator<<(muse::logger::Stream& s, const au::processing::ClipKey& k)
{
    s << "{trackId: " << k.trackId << ", clip: " << k.index << "}";
    return s;
}
