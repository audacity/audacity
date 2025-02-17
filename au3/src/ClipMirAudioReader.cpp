/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipMirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipMirAudioReader.h"
#include "ClipInterface.h"
#include "WaveClip.h"

#include <cassert>

ClipMirAudioReader::ClipMirAudioReader(
    std::optional<LibFileFormats::AcidizerTags> tags, std::string filename,
    WaveTrack& singleClipWaveTrack)
    : tags{std::move(tags)}
    , filename{std::move(filename)}
    , clip(*singleClipWaveTrack.Intervals().begin())
    , mClip(singleClipWaveTrack.GetClipInterfaces()[0])
{
}

double ClipMirAudioReader::GetSampleRate() const
{
    return mClip->GetRate();
}

long long ClipMirAudioReader::GetNumSamples() const
{
    return mClip->GetVisibleSampleCount().as_long_long();
}

void ClipMirAudioReader::ReadFloats(
    float* buffer, long long where, size_t numFrames) const
{
    std::fill(buffer, buffer + numFrames, 0.f);
    AddChannel(0, buffer, where, numFrames);
    if (mClip->NChannels() == 1) {
        return;
    }
    AddChannel(1, buffer, where, numFrames);
    std::transform(
        buffer, buffer + numFrames, buffer, [](float f) { return f / 2; });
}

void ClipMirAudioReader::AddChannel(
    size_t iChannel, float* buffer, sampleCount start, size_t len) const
{
    constexpr auto mayThrow = false;
    const auto iCache = mUseFirst[iChannel] ? 0 : 1;
    auto& cache = mCache[iChannel][iCache];
    auto view = mClip->GetSampleView(iChannel, start, len, mayThrow);
    cache.emplace(std::move(view));
    cache->AddTo(buffer, len);
    mUseFirst[iChannel] = !mUseFirst[iChannel];
}
