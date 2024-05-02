/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROСESSING_PROСESSINGTYPES_H
#define AU_PROСESSING_PROСESSINGTYPES_H

#include "global/realfn.h"

namespace au::processing {
using msecs_t = int64_t;
using secs_t = int64_t;
using samples_t = uint64_t;
using sample_rate_t = uint64_t;
using audioch_t = uint8_t;
using volume_db_t = float;
using volume_dbfs_t = float;
using gain_t = float;
using balance_t = float;

using TrackId = int32_t;

struct AudioOutputParams {
    volume_db_t volume = 0.f;
    balance_t balance = 0.f;
    bool solo = false;
    bool muted = false;
    bool forceMute = false;

    bool operator ==(const AudioOutputParams& other) const
    {
        return muse::RealIsEqual(volume, other.volume)
               && muse::RealIsEqual(balance, other.balance)
               && solo == other.solo
               && muted == other.muted
               && forceMute == other.forceMute;
    }
};
}

#endif // AU_PROСESSING_PROСESSINGTYPES_H
