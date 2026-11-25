/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchStretcher.cpp

**********************************************************************/
#include "TimeStretchers.h"

#if USE_SOUNDTOUCH

#include "AudioContainer.h"

#include <algorithm>
#include <vector>

// See SoundTouchBase.cpp: SoundTouch redefines these.
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT
#undef PACKAGE
#undef VERSION
#include "SoundTouch.h"

namespace {
constexpr size_t blockSize = 1024;

class SoundTouchStretcher final : public TimeAndPitchSource
{
public:
    SoundTouchStretcher(int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
        : mUpstream{upstream}
        , mNumChannels{numChannels}
        , mIn(blockSize, static_cast<int>(numChannels))
        , mInterleaved(blockSize * numChannels)
    {
        mSoundTouch.setSampleRate(static_cast<unsigned>(sampleRate));
        mSoundTouch.setChannels(static_cast<unsigned>(numChannels));
        // Tempo is speed: the inverse of the time ratio
        mSoundTouch.setTempo(static_cast<float>(1.0 / timeRatio));
    }

    void Pull(float* const* buffers, size_t samplesPerChannel) override
    {
        size_t done = 0;
        while (done < samplesPerChannel) {
            if (mSoundTouch.numSamples() == 0) {
                Feed();
                continue;
            }
            const auto wanted = std::min(samplesPerChannel - done, blockSize);
            const auto got = mSoundTouch.receiveSamples(mInterleaved.data(), static_cast<unsigned>(wanted));
            for (size_t i = 0; i < got; ++i) {
                for (size_t ch = 0; ch < mNumChannels; ++ch) {
                    buffers[ch][done + i] = mInterleaved[i * mNumChannels + ch];
                }
            }
            done += got;
        }
    }

private:
    void Feed()
    {
        mUpstream.Pull(mIn.Get(), blockSize);
        for (size_t i = 0; i < blockSize; ++i) {
            for (size_t ch = 0; ch < mNumChannels; ++ch) {
                mInterleaved[i * mNumChannels + ch] = mIn.Get()[ch][i];
            }
        }
        mSoundTouch.putSamples(mInterleaved.data(), static_cast<unsigned>(blockSize));
    }

    TimeAndPitchSource& mUpstream;
    const size_t mNumChannels;
    soundtouch::SoundTouch mSoundTouch;
    AudioContainer mIn;
    std::vector<float> mInterleaved;
};
} // namespace

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreateSoundTouch(
    int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio)
{
    return std::make_unique<SoundTouchStretcher>(sampleRate, numChannels, upstream, timeRatio);
}

#else

std::unique_ptr<TimeAndPitchSource> TimeStretchers::CreateSoundTouch(
    int, size_t, TimeAndPitchSource&, double)
{
    return nullptr;
}

#endif
