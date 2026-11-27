/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

#include "audio/iaudiometer.h"

namespace au::au4audio {
class MeterTapNode;

//! Sends to a meter what its taps kept of what plays, as it plays
/*!
 * On the device thread, which renders every tap: once a block has rendered, they have all kept it.
 */
class MeterSender
{
public:
    struct Tap {
        std::shared_ptr<MeterTapNode> node;
        //! Its meter: a track's, or the main one
        std::optional<audio::IAudioMeter::TrackId> trackId;
        //! How far ahead of the stream index it keeps what is heard there: the tracks', by the master effects' latency
        int64_t lead = 0;
    };

    explicit MeterSender(std::vector<Tap> taps);

    //! Sends what the taps kept of the `frames` frames that play from stream index `index`, heard from `time` on; then
    //! resets them. What a tap didn't keep all of, e.g. as reading jumped, isn't shown.
    void send(audio::IAudioMeter& meter, int64_t index, size_t frames, audio::TimePoint time) const;

private:
    const std::vector<Tap> m_taps;
};
}
