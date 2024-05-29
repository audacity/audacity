/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_AUDIOTYPES_H
#define AU_AU3WRAP_AUDIOTYPES_H

#include <string>

#include "global/types/number.h"
#include "global/realfn.h"
#include "global/async/channel.h"

namespace au::audio {
using msecs_t = int64_t;
using secs_t = muse::number_t<double>;
using samples_t = uint64_t;
using sample_rate_t = uint64_t;
using audioch_t = uint8_t;
using volume_db_t = float;
using volume_dbfs_t = float;
using gain_t = float;
using balance_t = float;

using TrackSequenceId = int32_t;
using TrackSequenceIdList = std::vector<TrackSequenceId>;

using TrackId = int32_t;
using TrackIdList = std::vector<TrackId>;
using TrackName = std::string;

using aux_channel_idx_t = uint8_t;

static constexpr TrackId INVALID_TRACK_ID = -1;

static constexpr int MINIMUM_BUFFER_SIZE = 1024;

static constexpr volume_dbfs_t MAX_DISPLAYED_DBFS = 0.f; // 100%
static constexpr volume_dbfs_t MIN_DISPLAYED_DBFS = -60.f; // 0%

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

struct AudioSignalVal {
    float amplitude = 0.f;
    volume_dbfs_t pressure = 0.f;
};

using AudioSignalChanges = muse::async::Channel<audioch_t, AudioSignalVal>;

struct AudioSignalsNotifier {
    void updateSignalValues(const audioch_t audioChNumber, const float newAmplitude, const volume_dbfs_t newPressure)
    {
        AudioSignalVal& signalVal = m_signalValuesMap[audioChNumber];

        volume_dbfs_t validatedPressure = std::max(newPressure, MINIMUM_OPERABLE_DBFS_LEVEL);

        if (muse::RealIsEqual(signalVal.pressure, validatedPressure)) {
            return;
        }

        if (std::abs(signalVal.pressure - validatedPressure) < PRESSURE_MINIMAL_VALUABLE_DIFF) {
            return;
        }

        signalVal.amplitude = newAmplitude;
        signalVal.pressure = validatedPressure;

        audioSignalChanges.send(audioChNumber, signalVal);
    }

    AudioSignalChanges audioSignalChanges;

private:
    static constexpr volume_dbfs_t PRESSURE_MINIMAL_VALUABLE_DIFF = 2.5f;
    static constexpr volume_dbfs_t MINIMUM_OPERABLE_DBFS_LEVEL = -100.f;

    std::map<audioch_t, AudioSignalVal> m_signalValuesMap;
};

enum class PlaybackStatus {
    Stopped = 0,
    Paused,
    Running
};
}

#endif // AU_AU3WRAP_AUDIOTYPES_H
