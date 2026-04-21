/*
* Audacity: A Digital Audio Editor
*/

#include "lteaudioreaderbridge.h"

#include "au3-stretching-sequence/ClipInterface.h"
#include "au3-math/SampleCount.h"

#include <algorithm>

#include "log.h"

using namespace au::importexport;

LteAudioReaderBridge::LteAudioReaderBridge(std::shared_ptr<const ClipInterface> clip)
    : m_clip(std::move(clip))
{
}

double LteAudioReaderBridge::GetSampleRate() const
{
    return m_clip->GetRate();
}

long long LteAudioReaderBridge::GetNumSamples() const
{
    return m_clip->GetVisibleSampleCount().as_long_long();
}

void LteAudioReaderBridge::ReadFloats(float* buffer, long long where, size_t numFrames) const
{
    IF_ASSERT_FAILED(where >= 0) {
        return;
    }
    IF_ASSERT_FAILED(where + static_cast<long long>(numFrames) <= GetNumSamples()) {
        return;
    }

    std::fill(buffer, buffer + numFrames, 0.f);
    addChannel(0, buffer, where, numFrames);
    if (m_clip->NChannels() == 1) {
        return;
    }
    // Average stereo channels to mono
    addChannel(1, buffer, where, numFrames);
    std::transform(buffer, buffer + numFrames, buffer, [](float f) { return f / 2; });
}

void LteAudioReaderBridge::addChannel(size_t iChannel, float* buffer, long long start, size_t len) const
{
    constexpr auto mayThrow = false;
    const auto iCache = m_useFirst[iChannel] ? 0 : 1;
    auto& cache = m_cache[iChannel][iCache];
    auto view = m_clip->GetSampleView(iChannel, sampleCount(start), len, mayThrow);
    cache.emplace(std::move(view));
    cache->AddTo(buffer, len);
    m_useFirst[iChannel] = !m_useFirst[iChannel];
}
