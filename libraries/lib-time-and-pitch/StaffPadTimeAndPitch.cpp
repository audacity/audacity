#include "StaffPadTimeAndPitch.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <memory>

namespace
{
// Let's use StaffPad's default value. (We have to reproduce it here as it has
// to be specified in the `setup` call.)
constexpr auto maxBlockSize = 1024;

void GetOffsetBuffer(
   float** offsetBuffer, float* const* buffer, size_t numChannels,
   size_t offset)
{
   for (auto i = 0u; i < numChannels; ++i)
      offsetBuffer[i] = buffer[i] + offset;
}

std::unique_ptr<staffpad::TimeAndPitch> MaybeCreateTimeAndPitch(
   int sampleRate, size_t numChannels,
   const TimeAndPitchInterface::Parameters& params)
{
   const auto timeRatio = params.timeRatio.value_or(1.);
   const auto pitchRatio = params.pitchRatio.value_or(1.);
   if (
      TimeAndPitchInterface::IsPassThroughMode(timeRatio) &&
      TimeAndPitchInterface::IsPassThroughMode(pitchRatio))
   {
      return nullptr;
   }
   auto timeAndPitch = std::make_unique<staffpad::TimeAndPitch>(sampleRate);
   timeAndPitch->setup(static_cast<int>(numChannels), maxBlockSize);
   timeAndPitch->setTimeStretchAndPitchFactor(timeRatio, pitchRatio);
   return timeAndPitch;
}
} // namespace

StaffPadTimeAndPitch::StaffPadTimeAndPitch(
   int sampleRate, size_t numChannels, TimeAndPitchSource& audioSource,
   const Parameters& parameters)
    : mAudioSource(audioSource)
    , mReadBuffer(maxBlockSize, numChannels)
    , mSampleRate(sampleRate)
    , mNumChannels(numChannels)
    , mTimeRatio(parameters.timeRatio.value_or(1.))
    , mPitchRatio(parameters.pitchRatio.value_or(1.))
    , mTimeAndPitch(
         MaybeCreateTimeAndPitch(sampleRate, numChannels, parameters))
{
   BootStretcher();
}

void StaffPadTimeAndPitch::GetSamples(float* const* output, size_t outputLen)
{
   if (!mTimeAndPitch)
      // Pass-through
      return mAudioSource.Pull(output, outputLen);

   std::lock_guard<std::mutex> lock(mTimeAndPitchMutex);
   // No need to re-check for `mTimeAndPitch`, because once set it's never
   // unset.

   auto numOutputSamples = 0u;
   while (numOutputSamples < outputLen)
   {
      if (IllState())
      {
         for (auto i = 0u; i < mNumChannels; ++i)
            std::fill_n(
               output[i] + numOutputSamples, outputLen - numOutputSamples, 0.f);
         return;
      }
      auto numOutputSamplesAvailable =
         mTimeAndPitch->getNumAvailableOutputSamples();
      while (numOutputSamplesAvailable <= 0)
      {
         auto numRequired = mTimeAndPitch->getSamplesToNextHop();
         while (numRequired > 0)
         {
            const auto numSamplesToFeed = std::min(numRequired, maxBlockSize);
            mAudioSource.Pull(mReadBuffer.Get(), numSamplesToFeed);
            mTimeAndPitch->feedAudio(mReadBuffer.Get(), numSamplesToFeed);
            numRequired -= numSamplesToFeed;
         }
         numOutputSamplesAvailable =
            mTimeAndPitch->getNumAvailableOutputSamples();
      }
      while (numOutputSamples < outputLen && numOutputSamplesAvailable > 0)
      {
         const auto numSamplesToGet =
            std::min({ maxBlockSize, numOutputSamplesAvailable,
                       static_cast<int>(outputLen - numOutputSamples) });
         // More-than-stereo isn't supported
         assert(mNumChannels <= 2);
         float* buffer[2] {};
         GetOffsetBuffer(buffer, output, mNumChannels, numOutputSamples);
         mTimeAndPitch->retrieveAudio(buffer, numSamplesToGet);
         numOutputSamplesAvailable -= numSamplesToGet;
         numOutputSamples += numSamplesToGet;
      }
   }
}

void StaffPadTimeAndPitch::OnCentShiftChange(int cents)
{
   std::lock_guard<std::mutex> lock(mTimeAndPitchMutex);
   // We don't unset `mTimeAndPitch` here, only reset it. If for some reason
   // this needs to change, then `BootStretcher` and `GetSamples` need
   // double-checked locking.

   mPitchRatio = std::pow(2., cents / 1200.);
   if (!mTimeAndPitch)
      mTimeAndPitch = MaybeCreateTimeAndPitch(
         mSampleRate, mNumChannels,
         TimeAndPitchInterface::Parameters { mTimeRatio, mPitchRatio });
   else
      mTimeAndPitch->setTimeStretchAndPitchFactor(mTimeRatio, mPitchRatio);
}

void StaffPadTimeAndPitch::BootStretcher()
{
   if (!mTimeAndPitch)
      // Bypass
      return;

   std::lock_guard<std::mutex> lock(mTimeAndPitchMutex);
   // No need to re-check for `mTimeAndPitch`, because once set it's never
   // unset.

   auto numOutputSamplesToDiscard =
      mTimeAndPitch->getLatencySamplesForStretchRatio(mTimeRatio * mPitchRatio);
   AudioContainer container(maxBlockSize, mNumChannels);
   while (numOutputSamplesToDiscard > 0)
   {
      if (IllState())
         return;
      auto numRequired = mTimeAndPitch->getSamplesToNextHop();
      while (numRequired > 0)
      {
         const auto numSamplesToFeed = std::min(maxBlockSize, numRequired);
         mAudioSource.Pull(container.Get(), numSamplesToFeed);
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

bool StaffPadTimeAndPitch::IllState() const
{
   // It doesn't require samples, yet it doesn't have output samples available.
   // Note that this must not be a permanent state, and may recover if the user
   // changes the pitch shift.
   // TODO: try to fix this in the stretcher implementation.
   return mTimeAndPitch->getSamplesToNextHop() <= 0 &&
          mTimeAndPitch->getNumAvailableOutputSamples() <= 0;
}
