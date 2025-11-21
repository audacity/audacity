/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <optional>
#include <memory>

using TimePoint = std::chrono::steady_clock::time_point;

class AUDIO_DEVICES_API IMeterSender
{
public:
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

    virtual ~IMeterSender() = default;
    virtual void push(uint8_t channel, const InterleavedSampleData& sampleData, const std::optional<TrackId>& = std::nullopt) = 0;
    virtual void start(double sampleRate) = 0;
    virtual void stop() = 0;
};

using IMeterSenderPtr = std::shared_ptr<IMeterSender>;
