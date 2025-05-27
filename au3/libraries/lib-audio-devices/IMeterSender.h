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
        const float* const buffer;
        const size_t frames;
        const size_t nChannels;
    };

    struct Sample
    {
        float peak;
        float rms;
    };

    virtual ~IMeterSender() = default;
    virtual void push(uint8_t channel, const Sample& sample, int64_t key = MASTER_TRACK_ID) = 0;
    virtual void push(uint8_t channel, const InterleavedSampleData& sampleData, int64_t key = MASTER_TRACK_ID) = 0;
    virtual void sendAll() = 0;
    virtual void reset() = 0;
    virtual void reserve(size_t size) = 0;
};
