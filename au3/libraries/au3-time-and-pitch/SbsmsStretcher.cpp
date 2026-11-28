/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SbsmsStretcher.cpp

**********************************************************************/
#include "TimeStretchers.h"

#if USE_SBSMS

#include "AudioContainer.h"

#include <sbsms.h>

#include <algorithm>
#include <memory>
#include <optional>
#include <vector>

namespace {
constexpr long outBlockSize = 512;

class SbsmsStretcher final : public TimeAndPitchSource
{
public:
    SbsmsStretcher(size_t numChannels, TimeAndPitchSource& upstream, double timeRatio, long long numInputSamples)
        : mNumChannels{numChannels}
        // A rate is a speed: the inverse of the time ratio
        , mRateSlide{_sbsms_::SlideConstant, static_cast<float>(1.0 / timeRatio), static_cast<float>(1.0 / timeRatio)}
        , mPitchSlide{_sbsms_::SlideConstant, 1.0f, 1.0f}
        , mQuality{&_sbsms_::SBSMSQualityStandard}
        , mSbsms{static_cast<int>(numChannels), &mQuality, true}
        , mInput{&mRateSlide, &mPitchSlide, numInputSamples, &mQuality, upstream, numChannels}
        , mOut{std::make_unique<_sbsms_::audio[]>(outBlockSize)}
    {
    }

    void Pull(float* const* buffers, size_t samplesPerChannel) override
    {
        size_t done = 0;
        while (done < samplesPerChannel) {
            const auto wanted = std::min<long>(static_cast<long>(samplesPerChannel - done), outBlockSize);
            const auto got = mSbsms.read(&mInput, mOut.get(), wanted);
            if (got <= 0) {
                // All the input has come out
                for (size_t ch = 0; ch < mNumChannels; ++ch) {
                    std::fill(buffers[ch] + done, buffers[ch] + samplesPerChannel, 0.f);
                }
                return;
            }
            for (long i = 0; i < got; ++i) {
                for (size_t ch = 0; ch < mNumChannels; ++ch) {
                    buffers[ch][done + i] = mOut[i][ch];
                }
            }
            done += got;
        }
    }

private:
    //! SBSMS pulls its input through this
    class Input final : public _sbsms_::SBSMSInterfaceSliding
    {
    public:
        Input(_sbsms_::Slide* rateSlide, _sbsms_::Slide* pitchSlide, long long numInputSamples,
              _sbsms_::SBSMSQuality* quality, TimeAndPitchSource& upstream, size_t numChannels)
            : SBSMSInterfaceSliding(rateSlide, pitchSlide, false, numInputSamples, 0, quality)
            , mUpstream{upstream}
            , mNumChannels{numChannels}
        {
        }

        long samples(_sbsms_::audio* buf, long n) override
        {
            if (!mScratch || mScratchSize < n) {
                mScratch.emplace(static_cast<int>(n), static_cast<int>(mNumChannels));
                mScratchSize = n;
            }
            mUpstream.Pull(mScratch->Get(), static_cast<size_t>(n));
            const float* const left = mScratch->Get()[0];
            const float* const right = mScratch->Get()[mNumChannels > 1 ? 1 : 0];
            for (long i = 0; i < n; ++i) {
                buf[i][0] = left[i];
                buf[i][1] = right[i];
            }
            return n;
        }

    private:
        TimeAndPitchSource& mUpstream;
        const size_t mNumChannels;
        std::optional<AudioContainer> mScratch;
        long mScratchSize = 0;
    };

    const size_t mNumChannels;
    _sbsms_::Slide mRateSlide;
    _sbsms_::Slide mPitchSlide;
    _sbsms_::SBSMSQuality mQuality;
    _sbsms_::SBSMS mSbsms;
    Input mInput;
    const std::unique_ptr<_sbsms_::audio[]> mOut;
};

//! When there's nothing left to stretch
class Silence final : public TimeAndPitchSource
{
public:
    explicit Silence(size_t numChannels)
        : mNumChannels{numChannels}
    {
    }

    void Pull(float* const* buffers, size_t samplesPerChannel) override
    {
        for (size_t ch = 0; ch < mNumChannels; ++ch) {
            std::fill(buffers[ch], buffers[ch] + samplesPerChannel, 0.f);
        }
    }

private:
    const size_t mNumChannels;
};
} // namespace

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreateSbsms(
    size_t numChannels, TimeAndPitchSource& upstream, double timeRatio, long long numInputSamples)
{
    if (numInputSamples <= 0) {
        return std::make_unique<Silence>(numChannels);
    }
    return std::make_unique<SbsmsStretcher>(numChannels, upstream, timeRatio, numInputSamples);
}

#else

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreateSbsms(
    size_t, TimeAndPitchSource&, double, long long)
{
    return nullptr;
}

#endif
