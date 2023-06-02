#include "WaveClipProcessor.h"
#include "Sequence.h"
#include "StaffPadTimeAndPitch.h"

#include <cassert>

using namespace std::placeholders;

namespace
{
std::unique_ptr<StaffPadTimeAndPitch>
CreateStretcher(double timeRatio, TimeAndPitchSource& src)
{
   TimeAndPitchInterface::Parameters params;
   params.timeRatio = timeRatio;
   constexpr auto numChannels = 1u; // for now assuming mono
   return std::make_unique<StaffPadTimeAndPitch>(numChannels, src, params);
}
} // namespace

WaveClipProcessor::WaveClipProcessor(
   const WaveClip& clip, double offsetFromPlayStartTime)
    : mClip(clip)
    , mReadPos(mClip.GetClosestSampleIndex(offsetFromPlayStartTime))
    , mStretcher(CreateStretcher(mClip.GetPlayoutStretchRatio(), *this))
    , mTotalNumSamplesToProduce(
         (mClip.GetPlayDuration() - offsetFromPlayStartTime) * mClip.GetRate())
{
}

size_t WaveClipProcessor::Process(
   float* const* buffer, size_t numChannels, size_t samplesPerChannel)
{
   const auto numSamplesToProduce =
      std::min(
         sampleCount { samplesPerChannel },
         mTotalNumSamplesToProduce - mTotalNumSamplesProduced)
         .as_size_t();
   mStretcher->GetSamples(buffer, numSamplesToProduce);
   mTotalNumSamplesProduced += numSamplesToProduce;
   return numSamplesToProduce;
}

bool WaveClipProcessor::SamplesRemaining() const
{
   return mTotalNumSamplesProduced < mTotalNumSamplesToProduce;
}

void WaveClipProcessor::Pull(
   float* const* buffers, size_t numChannels, size_t samplesPerChannel)
{
   // For now assuming mono.
   assert(numChannels == 1u);
   const auto initialReadPos = mReadPos;
   while (mReadPos - initialReadPos < samplesPerChannel)
   {
      const auto remainingSamplesInClip = GetRemainingSamplesInClip();
      if (remainingSamplesInClip <= 0)
         break;
      const auto readSoFar = (mReadPos - initialReadPos).as_size_t();
      const auto bestBlockSize =
         mClip.GetSequence()->GetBestBlockSize(mReadPos);
      const auto numSamplesToRead =
         std::min({ sampleCount { samplesPerChannel - readSoFar },
                    remainingSamplesInClip, sampleCount { bestBlockSize } })
            .as_size_t();
      constexpr auto mayThrow = false;
      if (mClip.GetSamples(
             reinterpret_cast<char*>(buffers[0u] + readSoFar), floatSample,
             mReadPos, numSamplesToRead, mayThrow))
         mReadPos += numSamplesToRead;
      else
         break;
   }
   const auto numRead = (mReadPos - initialReadPos).as_size_t();
   for (auto i = 0u; i < numChannels; ++i)
      std::fill(buffers[i] + numRead, buffers[i] + samplesPerChannel, 0.f);
}

sampleCount WaveClipProcessor::GetRemainingSamplesInClip() const
{
   return mClip.GetPlaySamplesCount() - mReadPos;
}
