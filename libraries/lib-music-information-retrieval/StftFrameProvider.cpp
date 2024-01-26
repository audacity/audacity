/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StftFrameProvider.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "StftFrameProvider.h"
#include "MirAudioReader.h"
#include "MirUtils.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <numeric>

namespace MIR
{
namespace
{
constexpr auto twoPi = 2 * 3.14159265358979323846;

int GetFrameSize(int sampleRate)
{
   // 2048 frame size for sample rate 44.1kHz
   return 1 << (11 + (int)std::round(std::log2(sampleRate / 44100.)));
}

double GetHopSize(int sampleRate, long long numSamples)
{
   // Aim for a hop size closest to 10ms, yet dividing `numSamples` to a power
   // of two. This will spare us the need for resampling when we need to get the
   // autocorrelation of the ODF using an FFT.
   const auto idealHopSize = 0.01 * sampleRate;
   const int exponent = std::round(std::log2(numSamples / idealHopSize));
   if (exponent < 0)
      return 0;
   const auto numFrames = 1 << exponent;
   return 1. * numSamples / numFrames;
}
} // namespace

StftFrameProvider::StftFrameProvider(const MirAudioReader& audio)
    : mAudio { audio }
    , mFftSize { GetFrameSize(audio.GetSampleRate()) }
    , mHopSize { GetHopSize(audio.GetSampleRate(), audio.GetNumSamples()) }
    , mWindow { GetNormalizedHann(mFftSize) }
    , mNumFrames { mHopSize > 0 ? static_cast<int>(std::round(
                                     audio.GetNumSamples() / mHopSize)) :
                                  0 }
    , mNumSamples { audio.GetNumSamples() }
{
   assert(mNumFrames == 0 || IsPowOfTwo(mNumFrames));
}

bool StftFrameProvider::GetNextFrame(std::vector<float>& frame)
{
   if (mNumFramesProvided >= mNumFrames)
      return false;
   frame.resize(mFftSize, 0.f);
   const int firstReadPosition = mHopSize - mFftSize;
   int start = std::round(firstReadPosition + mNumFramesProvided * mHopSize);
   while (start < 0)
      start += mNumSamples;
   const auto end = std::min<long long>(start + mFftSize, mNumSamples);
   const auto numToRead = end - start;
   mAudio.ReadFloats(frame.data(), start, numToRead);
   // It's not impossible that some user drops a file so short that `mFftSize >
   // mNumSamples`. In that case we won't be returning a meaningful
   // STFT, but that's a use case we're not interested in. We just need to make
   // sure we don't crash.
   const auto numRemaining = std::min(mFftSize - numToRead, mNumSamples);
   if (numRemaining > 0)
      mAudio.ReadFloats(frame.data() + numToRead, 0, numRemaining);
   std::transform(
      frame.begin(), frame.end(), mWindow.begin(), frame.begin(),
      std::multiplies<float>());
   ++mNumFramesProvided;
   return true;
}

void StftFrameProvider::SkipFrames(int numFrames)
{
   mNumFramesProvided += numFrames;
}

int StftFrameProvider::GetNumFrames() const
{
   return mNumFrames;
}

int StftFrameProvider::GetSampleRate() const
{
   return mAudio.GetSampleRate();
}

double StftFrameProvider::GetFrameRate() const
{
   return 1. * mAudio.GetSampleRate() / mHopSize;
}

int StftFrameProvider::GetFftSize() const
{
   return mFftSize;
}
} // namespace MIR
