/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/iclipchannelreader.h"

#include "au3-stretching-sequence/AudioSegmentSampleView.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::spectrogram {
class Au3ClipChannelReader final : public IClipChannelReader
{
public:
    Au3ClipChannelReader(const WaveChannelInterval& waveChannelInterval);
    ~Au3ClipChannelReader() override = default;

    void readSamples(long long from, int myLen, float* destination, bool mayThrow) override;

private:
    const WaveChannelInterval& m_waveChannelInterval;
    std::optional<AudioSegmentSampleView> m_sampleCacheHolder;
};
}
