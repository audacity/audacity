/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/iclipchannelreader.h"

#include "au3-stretching-sequence/AudioSegmentSampleView.h"
#include "au3-wave-track/WaveTrack.h"

#include <mutex>
#include <vector>

class WaveClipChannel;

namespace au::spectrogram {
class Au3ConcurrentClipChannelReader final : public IClipChannelReader
{
public:
    Au3ConcurrentClipChannelReader(const WaveChannelInterval&);
    ~Au3ConcurrentClipChannelReader() override = default;

    void readSamples(long long from, int myLen, float* destination, bool mayThrow) override;

private:
    const WaveChannelInterval& m_waveChannelInterval;
    std::vector<AudioSegmentSampleView> m_sampleCacheHolders;
    std::mutex m_sampleCacheMutex;
};
}
