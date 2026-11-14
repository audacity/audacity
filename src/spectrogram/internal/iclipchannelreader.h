/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
class IClipChannelReader
{
public:
    virtual ~IClipChannelReader() = default;
    virtual void readSamples(long long from, int myLen, float* destination, bool mayThrow) = 0;
};
}
