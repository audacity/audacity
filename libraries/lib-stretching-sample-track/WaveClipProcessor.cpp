#include "WaveClipProcessor.h"
#include "Sequence.h"
#include "StaffPadTimeAndPitch.h"

#include <cassert>

using namespace std::placeholders;

WaveClipProcessor::WaveClipProcessor(const WaveClip& clip)
    : mClip(clip)
{
}

void WaveClipProcessor::SetOffsetFromPlayStartTime(double pstOffset)
{
   mReadPos = mClip.GetClosestSampleIndex(pstOffset);
   TimeAndPitchInterface::Parameters params;
   params.timeRatio = mClip.GetPlayoutStretchRatio();
   constexpr auto numChannels = 1u; // for now assuming mono
   mStretcher =
      std::make_unique<StaffPadTimeAndPitch>(numChannels, *this, params);
}

size_t WaveClipProcessor::Process(
   float* const* buffer, size_t numChannels, size_t samplesPerChannel)
{
   assert(mStretcher);
   if (!mStretcher)
   {
      return 0u;
   }
   mStretcher->GetSamples(buffer, samplesPerChannel);
   return samplesPerChannel;
}

bool WaveClipProcessor::SamplesRemaining() const
{
   return mStretcher->CanReturnMoreSamples();
}

size_t WaveClipProcessor::Pull(
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
   const auto readSoFar = (mReadPos - initialReadPos).as_size_t();
   // For convenience to the client, still fill the rest with zeros.
   std::fill(buffers[0u] + readSoFar, buffers[0u] + samplesPerChannel, 0.f);
   return readSoFar;
}

bool WaveClipProcessor::Empty() const
{
   return GetRemainingSamplesInClip() <= 0;
}

sampleCount WaveClipProcessor::GetRemainingSamplesInClip() const
{
   return mClip.GetPlaySamplesCount() - mReadPos;
}
