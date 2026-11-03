/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/iclipchannelreader.h"

#include "au3-clip-processing/AudioSegmentSampleView.h"
#include "au3-clip-processing/ClipFades.h"
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
    const ClipFades::Snapshot m_fades;
    std::vector<AudioSegmentSampleView> m_sampleCacheHolders;
    std::mutex m_sampleCacheMutex;
};
}
