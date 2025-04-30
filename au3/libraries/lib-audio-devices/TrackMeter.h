#pragma once

#include <cstdint>

class AUDIO_DEVICES_API TrackMeter
{
public:
    virtual ~TrackMeter() = default;

    virtual void Update(int64_t trackId, size_t channel, const float* sampleData, unsigned long numFrames) = 0;
};
