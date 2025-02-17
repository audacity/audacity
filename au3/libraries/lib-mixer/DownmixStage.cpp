/**********************************************************************

  Audacity: A Digital Audio Editor

  DownmixStage.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************/

#include "DownmixStage.h"

#include <cassert>
#include <numeric>

#include "SampleCount.h"
#include "DownmixSource.h"

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

namespace {
void MixBuffers(unsigned numChannels,
                const unsigned char* channelFlags, const float* volumes,
                const float& src, AudioGraph::Buffers& dests, int len)
{
    const auto pSrc = &src;
    for (unsigned int c = 0; c < numChannels; c++) {
        if (!channelFlags[c]) {
            continue;
        }
        auto dest = &dests.GetWritePosition(c);
        for (int j = 0; j < len; ++j) {
            dest[j] += pSrc[j] * volumes[c]; // the actual mixing process
        }
    }
}
}

DownmixStage::DownmixStage(std::vector<std::unique_ptr<DownmixSource> > downmixSources,
                           size_t numChannels,
                           size_t bufferSize,
                           ApplyVolume applyGain)
    : mDownmixSources(std::move(downmixSources))
    // PRL:  Bug2536: see other comments below for the last, padding argument
    // TODO: more-than-two-channels
    // Issue 3565 workaround:  allocate one extra buffer when applying a
    // GVerb effect stage.  It is simply discarded
    // See also issue 3854, when the number of out channels expected by the
    // plug-in is yet larger
    , mFloatBuffers{3, bufferSize, 1, 1}
    , mNumChannels(numChannels)
    , mApplyVolume(applyGain)
{
}

DownmixStage::~DownmixStage() = default;

bool DownmixStage::AcceptsBuffers(const Buffers& buffers) const
{
    return buffers.Channels() == mNumChannels
           && AcceptsBlockSize(buffers.BlockSize());
}

bool DownmixStage::AcceptsBlockSize(size_t blockSize) const
{
    return blockSize <= mFloatBuffers.BufferSize();
}

std::optional<size_t> DownmixStage::Acquire(Buffers& data, size_t maxToProcess)
{
    // TODO: more-than-two-channels
    auto maxChannels = std::max(2u, mFloatBuffers.Channels());
    const auto channelFlags = stackAllocate(unsigned char, mNumChannels);
    const auto volumes = stackAllocate(float, mNumChannels);
    if (mApplyVolume == ApplyVolume::Discard) {
        std::fill(volumes, volumes + mNumChannels, 1.0f);
    }

    size_t maxOut = 0;
    for (auto c = 0; c < data.Channels(); ++c) {
        data.ClearBuffer(c, maxToProcess);
    }

    for (auto& downmixSource : mDownmixSources) {
        auto oResult = downmixSource->GetDownstream().Acquire(mFloatBuffers, maxToProcess);
        // One of MixVariableRates or MixSameRate assigns into mTemp[*][*]
        // which are the sources for the CopySamples calls, and they copy into
        // mBuffer[*][*]
        if (!oResult) {
            return 0;
        }
        const auto result = *oResult;
        maxOut = std::max(maxOut, result);

        // Insert effect stages here!  Passing them all channels of the track

        const auto limit = std::min<size_t>(downmixSource->NChannels(), maxChannels);
        for (size_t j = 0; j < limit; ++j) {
            const auto pFloat = (const float*)mFloatBuffers.GetReadPosition(j);
            if (mApplyVolume != ApplyVolume::Discard) {
                for (size_t c = 0; c < mNumChannels; ++c) {
                    if (mNumChannels > 1) {
                        volumes[c] = downmixSource->GetChannelGain(c);
                    } else {
                        volumes[c] = downmixSource->GetChannelGain(j);
                    }
                }
                if (mApplyVolume == ApplyVolume::Mixdown
                    && mNumChannels == 1
                    && downmixSource->CanMakeMono()) {
                    volumes[0] /= static_cast<float>(limit);
                }
            }

            downmixSource->FindChannelFlags(channelFlags, mNumChannels, j);
            MixBuffers(mNumChannels, channelFlags, volumes, *pFloat, data, result);
        }

        downmixSource->GetDownstream().Release();
        mFloatBuffers.Advance(result);
        mFloatBuffers.Rotate();
    }

    // MB: this doesn't take warping into account, replaced with code based on mSamplePos
    //mT += (maxOut / mRate);

    assert(maxOut <= maxToProcess);
    return maxOut;
}

sampleCount DownmixStage::Remaining() const
{
    return std::accumulate(
        mDownmixSources.begin(), mDownmixSources.end(), sampleCount { 0 },
        [](sampleCount sum, const auto& source) {
        return std::max(sum, source->GetDownstream().Remaining());
    });
}

bool DownmixStage::Release()
{
    return true;
}
