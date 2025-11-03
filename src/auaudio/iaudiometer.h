/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "auaudio/auaudiotypes.h"

#include "framework/global/async/channel.h"

#include <chrono>
#include <memory>
#include <optional>

constexpr int64_t MASTER_TRACK_ID = -2;
using TimePoint = std::chrono::steady_clock::time_point;

namespace au::auaudio {
class IAudioMeter
{
public:
    static std::unique_ptr<IAudioMeter> create();

    struct InterleavedSampleData
    {
        InterleavedSampleData(
            const float* const buf,
            size_t frm,
            size_t nCh,
            TimePoint dacTime)
            : buffer(buf)
            , frames(frm)
            , nChannels(nCh)
            , dacTime(std::move(dacTime))
        {
        }

        const float* const buffer;
        const size_t frames;
        const size_t nChannels;
        const TimePoint dacTime;
    };

    struct TrackId {
        TrackId() = default;
        explicit TrackId(int64_t v)
            : value(v) {}
        int64_t value = -1;
        bool operator<(const TrackId& other) const { return value < other.value; }
    };

    using OptionalTimePoint = std::optional<TimePoint>;

    virtual ~IAudioMeter() = default;
    virtual void push(uint8_t channel, const InterleavedSampleData& sampleData, TrackId = TrackId { MASTER_TRACK_ID }) = 0;
    virtual void start(double sampleRate) = 0;
    virtual void stop() = 0;
    virtual muse::async::Channel<auaudio::audioch_t, auaudio::MeterSignal> dataChanged(TrackId key = TrackId { MASTER_TRACK_ID }) = 0;
};

using IAudioMeterPtr = std::shared_ptr<IAudioMeter>;
}
