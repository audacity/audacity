/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  VarispeedStretcher.cpp

**********************************************************************/
#include "TimeStretchers.h"

#include "AudioContainer.h"

#include "au3-math/Resample.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <vector>

namespace {
constexpr size_t blockSize = 1024;

//! Resamples, so that the pitch moves with the speed
class VarispeedStretcher final : public TimeAndPitchSource
{
public:
    VarispeedStretcher(size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
        : mUpstream{upstream}
        , mNumChannels{numChannels}
        , mTimeRatio{timeRatio}
        // Room for what a whole input block becomes, so that the resampler
        // never stops for want of space
        , mOutCapacity{static_cast<size_t>(std::ceil(blockSize * timeRatio)) + blockSize}
        , mIn(blockSize, static_cast<int>(numChannels))
        , mOut(static_cast<int>(mOutCapacity), static_cast<int>(numChannels))
    {
        mResamplers.reserve(numChannels);
        for (size_t i = 0; i < numChannels; ++i) {
            // Output samples per input sample is the time ratio
            mResamplers.emplace_back(true, timeRatio, timeRatio);
        }
    }

    void Pull(float* const* buffers, size_t samplesPerChannel) override
    {
        size_t done = 0;
        while (done < samplesPerChannel) {
            if (mOutPos == mOutLen && !Refill()) {
                for (size_t ch = 0; ch < mNumChannels; ++ch) {
                    std::fill(buffers[ch] + done, buffers[ch] + samplesPerChannel, 0.f);
                }
                return;
            }
            const auto count = std::min(samplesPerChannel - done, mOutLen - mOutPos);
            for (size_t ch = 0; ch < mNumChannels; ++ch) {
                std::copy_n(mOut.Get()[ch] + mOutPos, count, buffers[ch] + done);
            }
            mOutPos += count;
            done += count;
        }
    }

private:
    //! @return false if the resampler is stuck
    bool Refill()
    {
        mOutPos = mOutLen = 0;
        // The resampler holds back some output at first, until it has enough
        // input; a few blocks at most.
        for (auto attempts = 0; attempts < 64 && mOutLen == 0; ++attempts) {
            if (mInPos == mInLen) {
                mUpstream.Pull(mIn.Get(), blockSize);
                mInPos = 0;
                mInLen = blockSize;
            }
            size_t used = 0;
            for (size_t ch = 0; ch < mNumChannels; ++ch) {
                const auto [chUsed, chMade] = mResamplers[ch].Process(
                    mTimeRatio, mIn.Get()[ch] + mInPos, mInLen - mInPos, false,
                    mOut.Get()[ch], mOutCapacity);
                // Same ratio, same lengths: the channels stay in step
                assert(ch == 0 || (chUsed == used && chMade == mOutLen));
                used = chUsed;
                mOutLen = chMade;
            }
            mInPos += used;
        }
        assert(mOutLen > 0);
        return mOutLen > 0;
    }

    TimeAndPitchSource& mUpstream;
    const size_t mNumChannels;
    const double mTimeRatio;
    const size_t mOutCapacity;
    std::vector<Resample> mResamplers;
    AudioContainer mIn;
    size_t mInPos = 0;
    size_t mInLen = 0;
    AudioContainer mOut;
    size_t mOutPos = 0;
    size_t mOutLen = 0;
};
} // namespace

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreateVarispeed(
    size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
{
    return std::make_unique<VarispeedStretcher>(numChannels, upstream, timeRatio);
}
