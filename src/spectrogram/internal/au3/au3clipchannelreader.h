/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/iclipchannelreader.h"

#include "au3-clip-processing/AudioSegmentSampleView.h"
#include "au3-clip-processing/ClipFades.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::spectrogram {
void applyClipFades(const WaveChannelInterval& clip, const ClipFades::Snapshot& fades, long long from, int len, float* samples);

class Au3ClipChannelReader final : public IClipChannelReader
{
public:
    Au3ClipChannelReader(const WaveChannelInterval& waveChannelInterval);
    ~Au3ClipChannelReader() override = default;

    void readSamples(long long from, int myLen, float* destination, bool mayThrow) override;

private:
    const WaveChannelInterval& m_waveChannelInterval;
    const ClipFades::Snapshot m_fades;
    std::optional<AudioSegmentSampleView> m_sampleCacheHolder;
};
}
