/*
 * Audacity: A Digital Audio Editor
 */
#include "au3concurrentclipchannelreader.h"

#include "au3-wave-track/WaveClip.h"
#include "au3-math/SampleCount.h"

namespace au::spectrogram {
Au3ConcurrentClipChannelReader::Au3ConcurrentClipChannelReader(const WaveChannelInterval& waveChannelInterval)
    : m_waveChannelInterval(waveChannelInterval)
{
}

void Au3ConcurrentClipChannelReader::readSamples(long long from, int myLen, float* destination, bool mayThrow)
{
    //! Careful: GetSampleView may be declared `const`, the SqliteSampleBlock::GetFloatSampleView it calls is not.
    //! Hence it's probably safest to lock right from the start.
    std::lock_guard<std::mutex> lock(m_sampleCacheMutex);
    auto sampleView = m_waveChannelInterval.GetSampleView(sampleCount { from }, myLen, mayThrow);
    sampleView.Copy(destination, myLen);
    m_sampleCacheHolders.emplace_back(std::move(sampleView));
}
}
