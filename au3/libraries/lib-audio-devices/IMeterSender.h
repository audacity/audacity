/*
* Audacity: A Digital Audio Editor
*/
#pragma once

constexpr int64_t MASTER_TRACK_ID = -2;

class AUDIO_DEVICES_API IMeterSender
{
public:
    struct InterleavedSampleData
    {
        InterleavedSampleData(
            const float* const buf,
            size_t frm,
            size_t nCh,
            double firstTs,
            std::chrono::steady_clock::time_point when_)
            : buffer(buf)
            , frames(frm)
            , nChannels(nCh)
            , firstSampleTimestamp(firstTs)
            , when(when_)
        {
        }

        const float* const buffer;
        const size_t frames;
        const size_t nChannels;
        const double firstSampleTimestamp;
        const std::chrono::steady_clock::time_point when;
    };

    struct TrackId {
        TrackId() = default;
        explicit TrackId(int64_t v)
            : value(v) {}
        int64_t value = -1;
        bool operator<(const TrackId& other) const { return value < other.value; }
    };

    using OptionalTimePoint = std::optional<std::chrono::steady_clock::time_point>;

    virtual ~IMeterSender() = default;
    virtual void push(uint8_t channel, const InterleavedSampleData& sampleData, TrackId = TrackId { MASTER_TRACK_ID }) = 0;
    virtual void start(double sampleRate) = 0;
    virtual void stop() = 0;
};
