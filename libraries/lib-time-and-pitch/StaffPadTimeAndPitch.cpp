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
      // Pass-through
      return mAudioSource.Pull(output, outputLen);

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
            mAudioSource.Pull(mReadBuffer.Get(), numSamplesToFeed);
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
