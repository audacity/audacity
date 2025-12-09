/*
 * Audacity: A Digital Audio Editor
 */
#include "au3clipchannelreader.h"

#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::spectrogram {
Au3ClipChannelReader::Au3ClipChannelReader(const WaveChannelInterval& waveChannelInterval)
    : m_waveChannelInterval(waveChannelInterval)
{
}

void Au3ClipChannelReader::readSamples(long long from, int myLen, float* destination, bool mayThrow)
{
    m_sampleCacheHolder.emplace(m_waveChannelInterval.GetSampleView(from, myLen, mayThrow));
    m_sampleCacheHolder->Copy(destination, myLen);
}
} // namespace au::spectrogram
