/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/realfn.h"
#include "global/types/secs.h" // IWYU pragma: export

namespace au::audio {
using audioch_t = uint8_t;
using aux_channel_idx_t = uint8_t;
using volume_db_t = float; //! TODO use db_t
using volume_dbfs_t = float;
using gain_t = float;
using balance_t = float;
using samples_t = uint64_t; //! TODO use double
using sample_rate_t = uint64_t;

struct AudioSignalVal {
    float amplitude = 0.f;
    volume_dbfs_t pressure = 0.f;
};

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
