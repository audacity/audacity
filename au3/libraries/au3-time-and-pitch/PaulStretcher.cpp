/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PaulStretcher.cpp

  The algorithm of Nasca Octavian Paul's Paulstretch, as in Audacity's
  Paulstretch effect, made to stream.

**********************************************************************/
#include "TimeStretchers.h"

#include "AudioContainer.h"

#include "au3-fft/FFT.h"

#include <algorithm>
#include <cmath>
#include <random>
#include <vector>

namespace {
//! The Paulstretch effect's default "time resolution", in seconds
constexpr double timeResolution = 0.25;

constexpr float pi = 3.14159265358979f;

//! Half the analysis window: a power of two, as the effect computes it
size_t GetHalfWindowSize(int sampleRate)
{
    const auto exponent = std::floor(std::log2(sampleRate * timeResolution / 2.0) + 0.5);
    return std::max<size_t>(static_cast<size_t>(std::pow(2.0, exponent)), 128);
}

/*!
 * Each output hop of `H` samples comes from a `2H`-sample window of input whose
 * phases are randomized; the window moves `H / timeRatio` samples per hop.
 * Both channels get the same random phases, which keeps the stereo image.
 */
class PaulStretcher final : public TimeAndPitchSource
{
public:
    PaulStretcher(int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
        : mUpstream{upstream}
        , mNumChannels{numChannels}
        , mInputHop{GetHalfWindowSize(sampleRate) / timeRatio}
        , mHalf{GetHalfWindowSize(sampleRate)}
        , mWindowSize{2 * mHalf}
        , mWindow(mWindowSize, 1.f)
        , mPools(static_cast<int>(mWindowSize), static_cast<int>(numChannels))
        , mOldFrames(static_cast<int>(mWindowSize), static_cast<int>(numChannels))
        , mOut(static_cast<int>(mHalf), static_cast<int>(numChannels))
        , mScratch(static_cast<int>(mWindowSize), static_cast<int>(numChannels))
        , mPhases(mWindowSize / 2)
        , mFrame(mWindowSize)
        , mReal(mWindowSize)
        , mImag(mWindowSize)
        , mTmp(mWindowSize)
    {
        WindowFunc(eWinFuncHann, mWindowSize, mWindow.data());
    }

    void Pull(float* const* buffers, size_t samplesPerChannel) override
    {
        size_t done = 0;
        while (done < samplesPerChannel) {
            if (mOutPos == mHalf) {
                NextHop();
            }
            const auto count = std::min(samplesPerChannel - done, mHalf - mOutPos);
            for (size_t ch = 0; ch < mNumChannels; ++ch) {
                std::copy_n(mOut.Get()[ch] + mOutPos, count, buffers[ch] + done);
            }
            mOutPos += count;
            done += count;
        }
    }

private:
    void NextHop()
    {
        if (mFirstHop) {
            // Fill the whole window, and synthesize it once so that the first
            // output hop has a previous frame to cross-fade from.
            Feed(mWindowSize);
            Synthesize();
            mFirstHop = false;
        } else {
            mHopRemainder += mInputHop;
            const auto hop = static_cast<size_t>(std::floor(mHopRemainder));
            mHopRemainder -= hop;
            Feed(hop);
        }
        Synthesize();
        mOutPos = 0;
    }

    //! Moves the input window `count` samples on
    void Feed(size_t count)
    {
        // Sped up by more than 2x, the window skips some input altogether
        while (count > mWindowSize) {
            const auto skip = std::min(count - mWindowSize, mWindowSize);
            mUpstream.Pull(mScratch.Get(), skip);
            count -= skip;
        }
        if (count == 0) {
            return;
        }
        mUpstream.Pull(mScratch.Get(), count);
        for (size_t ch = 0; ch < mNumChannels; ++ch) {
            float* const pool = mPools.Get()[ch];
            std::copy(pool + count, pool + mWindowSize, pool);
            std::copy_n(mScratch.Get()[ch], count, pool + mWindowSize - count);
        }
    }

    void Synthesize()
    {
        std::uniform_real_distribution<float> phaseDistribution(0.f, 2.f * pi);
        for (auto& phase : mPhases) {
            phase = phaseDistribution(mRandom);
        }

        const auto N = mWindowSize;
        const auto H = mHalf;
        const float tmp = pi / H;
        constexpr float hinvSqrt2 = 0.853553390593f; // (1 + 1/sqrt(2)) / 2
        // Output hops overlap the same way whatever the speed
        constexpr float ampFactor = 2.f;

        for (size_t ch = 0; ch < mNumChannels; ++ch) {
            const float* const pool = mPools.Get()[ch];
            for (size_t i = 0; i < N; ++i) {
                mFrame[i] = pool[i] * mWindow[i];
            }
            RealFFT(N, mFrame.data(), mReal.data(), mImag.data());

            // Keep the magnitudes, randomize the phases
            for (size_t i = 1; i < N / 2; ++i) {
                const float magnitude = std::sqrt(mReal[i] * mReal[i] + mImag[i] * mImag[i]);
                const float c = magnitude * std::cos(mPhases[i]);
                const float s = magnitude * std::sin(mPhases[i]);
                mReal[i] = mReal[N - i] = c;
                mImag[i] = s;
                mImag[N - i] = -s;
            }
            mReal[0] = mImag[0] = 0.f;
            mReal[N / 2] = mImag[N / 2] = 0.f;

            FFT(N, true, mReal.data(), mImag.data(), mFrame.data(), mTmp.data());

            float* const old = mOldFrames.Get()[ch];
            float* const out = mOut.Get()[ch];
            for (size_t i = 0; i < H; ++i) {
                const float a = 0.5f + 0.5f * std::cos(i * tmp);
                const float sample = mFrame[i + H] * (1.f - a) + old[i] * a;
                out[i] = sample * (hinvSqrt2 - (1.f - hinvSqrt2) * std::cos(i * 2.f * tmp)) * ampFactor;
            }
            std::copy_n(mFrame.data(), N, old);
        }
    }

    TimeAndPitchSource& mUpstream;
    const size_t mNumChannels;
    //! Input samples per output hop; fractional, hence `mHopRemainder`
    const double mInputHop;
    double mHopRemainder = 0.0;
    const size_t mHalf;
    const size_t mWindowSize;
    std::vector<float> mWindow;
    AudioContainer mPools;
    AudioContainer mOldFrames;
    AudioContainer mOut;
    //! Starts empty: the first pull fills it
    size_t mOutPos = mHalf;
    AudioContainer mScratch;
    bool mFirstHop = true;
    std::minstd_rand mRandom;
    std::vector<float> mPhases;
    std::vector<float> mFrame;
    std::vector<float> mReal;
    std::vector<float> mImag;
    std::vector<float> mTmp;
};
} // namespace

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreatePaulstretch(
    int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
{
    return std::make_unique<PaulStretcher>(sampleRate, numChannels, upstream, timeRatio);
}
