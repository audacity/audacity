/*
 * Audacity: A Digital Audio Editor
 */
#include "au3clipchannelreader.h"

#include "au3-math/SampleCount.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

namespace au {
void spectrogram::applyClipFades(const WaveChannelInterval& clip, const ClipFades::Snapshot& fades, long long from, int len, float* samples)
{
    if (fades.fadeIn <= 0 && fades.fadeOut <= 0) {
        return;
    }
    // The fades are in play time, the samples aren't stretched
    const auto playSecondsPerSample = clip.GetStretchRatio() / clip.GetRate();
    for (auto i = 0; i < len; ++i) {
        samples[i] *= static_cast<float>(fades.gainAt((from + i) * playSecondsPerSample));
    }
}

namespace spectrogram {
Au3ClipChannelReader::Au3ClipChannelReader(const WaveChannelInterval& waveChannelInterval)
    : m_waveChannelInterval(waveChannelInterval)
    , m_fades(ClipFades::Get(waveChannelInterval.GetClip()).snapshot())
{
}

void Au3ClipChannelReader::readSamples(long long from, int myLen, float* destination, bool mayThrow)
{
    m_sampleCacheHolder.emplace(m_waveChannelInterval.GetSampleView(sampleCount { from }, myLen, mayThrow));
    m_sampleCacheHolder->Copy(destination, myLen);
    applyClipFades(m_waveChannelInterval, m_fades, from, myLen, destination);
}
}
} // namespace au::spectrogram
