#include "StaffPadTimeAndPitch.h"

#include <algorithm>
#include <cassert>
#include <memory>

namespace
{
constexpr auto maxBlockSize = 512;

std::vector<float*>
GetOffsetBuffer(float* const* buffer, size_t numChannels, size_t offset)
{
   std::vector<float*> offsetBuffer(numChannels);
   for (auto i = 0u; i < numChannels; ++i)
   {
      offsetBuffer[i] = buffer[i] + offset;
   }
   return offsetBuffer;
}

std::unique_ptr<staffpad::TimeAndPitch> MaybeCreateTimeAndPitch(
   size_t numChannels, const TimeAndPitchInterface::Parameters& params)
{
   const auto timeRatio = params.timeRatio.value_or(1.);
   const auto pitchRatio = params.pitchRatio.value_or(1.);
   if (timeRatio == 1. && pitchRatio == 1.)
   {
      return nullptr;
   }
   auto timeAndPitch = std::make_unique<staffpad::TimeAndPitch>();
   timeAndPitch->setup(static_cast<int>(numChannels), maxBlockSize);
   timeAndPitch->setTimeStretchAndPitchFactor(timeRatio, pitchRatio);
   return timeAndPitch;
}
} // namespace

StaffPadTimeAndPitch::StaffPadTimeAndPitch(
   size_t numChannels, TimeAndPitchSource& audioSource,
   const Parameters& parameters)
    : mAudioSource(audioSource)
    , mReadBuffer(maxBlockSize, numChannels)
    , mNumChannels(numChannels)
    , mTimeRatio(parameters.timeRatio.value_or(1.))
    , mTimeAndPitch(MaybeCreateTimeAndPitch(numChannels, parameters))
{
   BootStretcher();
}

void StaffPadTimeAndPitch::GetSamples(float* const* output, size_t outputLen)
{
   if (!mTimeAndPitch)
   {
      // Pass-through
      mAudioSource.Pull(output, mNumChannels, outputLen);
      return;
   }
   auto numOutputSamples = 0u;
   while (numOutputSamples < outputLen)
   {
      // One would expect that feeding `getSamplesToNextHop()` samples would
      // always bring mTimeAndPitch to return `getNumAvailableOutputSamples() >
      // 0`, right ? With pitch shifting I saw the opposite happening. Brief
      // debugging hinted that `getSamplesToNextHop` might indeed be yielding an
      // underestimation. In that case repeating the operation once should
      // produce the expected result, but let's not write and infinite loop for
      // that until we know for sure what the problem is.
      // todo(mhodgkinson) figure this out
      auto firstCall = true;
      auto numOutputSamplesAvailable =
         mTimeAndPitch->getNumAvailableOutputSamples();
      while (numOutputSamplesAvailable == 0)
      {
         auto numRequired = mTimeAndPitch->getSamplesToNextHop();
         while (numRequired > 0)
         {
            const auto numSamplesToFeed = std::min(numRequired, maxBlockSize);
            PullFromSource(mReadBuffer.Get(), numSamplesToFeed);
            mTimeAndPitch->feedAudio(mReadBuffer.Get(), numSamplesToFeed);
            numRequired -= numSamplesToFeed;
         }
         numOutputSamplesAvailable =
            mTimeAndPitch->getNumAvailableOutputSamples();
         if (!firstCall)
         {
            break;
         }
         firstCall = false;
      }
      while (numOutputSamples < outputLen && numOutputSamplesAvailable > 0)
      {
         const auto numSamplesToGet =
            std::min({ maxBlockSize, numOutputSamplesAvailable,
                       static_cast<int>(outputLen - numOutputSamples) });
         const auto buffer =
            GetOffsetBuffer(output, mNumChannels, numOutputSamples);
         mTimeAndPitch->retrieveAudio(buffer.data(), numSamplesToGet);
         numOutputSamplesAvailable -= numSamplesToGet;
         numOutputSamples += numSamplesToGet;
      }
   }
}

bool StaffPadTimeAndPitch::CanReturnMoreSamples() const
{
   // If our audio source isn't empty, we'll have samples to feed
   // `mTimeAndPitch` with, who in turn will yield some audio.

   // It can also be that our source is empty, but `mTimeAndPitch` still has
   // samples in its ISTFT buffer. We don't want to miss those ...

   // As for `mNumTrailingZeros`, those are zero-pad samples we had to feed
   // `mTimeAndPitch´ with such that it can make an STFT window out of it. Those
   // zeros will be seen by `mTimeAndPitch` as normal input, but for us it will
   // just be a tail of decaying near-zero values, a resonance of sort. We might
   // use it for cross-fading later, but for now just do as though it were exact
   // zeros and avoid delaying whatever `WaveClip` comes after.
   return !mAudioSource.Empty() ||
          (mTimeAndPitch &&
           mTimeAndPitch->getNumAvailableOutputSamples() > mNumTrailingZeros);
}

void StaffPadTimeAndPitch::PullFromSource(
   float* const* dst, size_t numSamplesToPull)
{
   const auto numSamplesPulled =
      mAudioSource.Pull(dst, mNumChannels, numSamplesToPull);
   mNumTrailingZeros += (numSamplesToPull - numSamplesPulled) * mTimeRatio;
}

void StaffPadTimeAndPitch::BootStretcher()
{
   if (!mTimeAndPitch)
   {
      // Bypass
      return;
   }
   const auto latencySamples = mTimeAndPitch->getLatencySamples();
   auto numOutputSamplesToDiscard =
      static_cast<int>(latencySamples * mTimeRatio + 0.5);
   AudioContainer container(maxBlockSize, mNumChannels);
   while (numOutputSamplesToDiscard > 0)
   {
      auto numRequired = mTimeAndPitch->getSamplesToNextHop();
      while (numRequired > 0)
      {
         const auto numSamplesToFeed = std::min(maxBlockSize, numRequired);
         PullFromSource(container.Get(), numSamplesToFeed);
         mTimeAndPitch->feedAudio(container.Get(), numSamplesToFeed);
         numRequired -= numSamplesToFeed;
      }
      const auto totalNumSamplesToRetrieve = std::min(
         mTimeAndPitch->getNumAvailableOutputSamples(),
         numOutputSamplesToDiscard);
      auto totalNumRetrievedSamples = 0;
      while (totalNumRetrievedSamples < totalNumSamplesToRetrieve)
      {
         const auto numSamplesToRetrieve = std::min(
            maxBlockSize, totalNumSamplesToRetrieve - totalNumRetrievedSamples);
         mTimeAndPitch->retrieveAudio(container.Get(), numSamplesToRetrieve);
         totalNumRetrievedSamples += numSamplesToRetrieve;
      }
      numOutputSamplesToDiscard -= totalNumSamplesToRetrieve;
   }
}
