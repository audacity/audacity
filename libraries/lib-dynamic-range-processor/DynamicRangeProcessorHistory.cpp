#include "DynamicRangeProcessorHistory.h"
#include "DynamicRangeProcessorTypes.h"
#include <algorithm>

DynamicRangeProcessorHistory::DynamicRangeProcessorHistory(
   double framesPerSecond)
    : mFramesPerSecond { framesPerSecond }
    , mHistory { std::make_shared<std::vector<Sample>>() }
{
}

bool DynamicRangeProcessorHistory::Push(
   const std::vector<DynamicRangeProcessorOutputSample>& samples)
{
   if (samples.empty())
      return false;

   const auto elapsed =
      mLastFrameCounter.has_value() ?
         (samples.back().frameCounter - *mLastFrameCounter) / mFramesPerSecond :
         0.0;
   mLastFrameCounter = samples.back().frameCounter;
   auto& hist = *mHistory;
   std::for_each(hist.begin(), hist.end(), [&](Sample& sample) {
      sample.time -= elapsed;
   });
   const int numNewSamples = samples.size();
   for (auto i = 0; i < numNewSamples; ++i)
   {
      const auto& sample = samples[i];
      const float t = (i + 1 - numNewSamples) / mFramesPerSecond;
      if (!hist.empty() && t <= hist.back().time)
         continue;
      hist.push_back(
         { t, sample.targetCompressionDb, sample.actualCompressionDb });
   }

   const auto it =
      std::find_if(hist.begin(), hist.end(), [](const Sample& sample) {
         return sample.time >= -maxTimeSeconds;
      });
   hist.erase(hist.begin(), it);

   return true;
}

std::shared_ptr<const std::vector<DynamicRangeProcessorHistory::Sample>>
DynamicRangeProcessorHistory::GetHistory() const
{
   return mHistory;
}

void DynamicRangeProcessorHistory::Reset()
{
   mHistory->clear();
}
