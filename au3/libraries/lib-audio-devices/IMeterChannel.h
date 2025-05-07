/*
* Audacity: A Digital Audio Editor
*/
#pragma once

constexpr int64_t MASTER_TRACK_ID = -2;

class AUDIO_DEVICES_API IMeterChannel
{
public:
    struct SampleData
    {
        const float* buffer;
        const size_t frames;
        const size_t step;
    };

    virtual ~IMeterChannel() = default;
    virtual void push(uint8_t channel, float signal, int64_t key = MASTER_TRACK_ID) = 0;
    virtual void push(uint8_t channel, const SampleData&& sampleData, int64_t key = MASTER_TRACK_ID) = 0;
    virtual void sendAll() = 0;
    virtual void reset() = 0;
    virtual void reserve(size_t size) = 0;
};
