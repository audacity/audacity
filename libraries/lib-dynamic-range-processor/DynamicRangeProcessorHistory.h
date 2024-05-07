// TODO header
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include <memory>
#include <optional>
#include <vector>

class DYNAMIC_RANGE_PROCESSOR_API DynamicRangeProcessorHistory final
{
public:
   DynamicRangeProcessorHistory(double framesPerSecond);

   struct Sample
   {
      float time = 0.f;
      float target = 0.f;
      float follower = 0.f;
   };

   static constexpr auto maxTimeSeconds = 10.f;

   bool Push(const std::vector<DynamicRangeProcessorOutputSample>& samples);
   std::shared_ptr<const std::vector<DynamicRangeProcessorHistory::Sample>>
   GetHistory() const;
   void Reset();

private:
   const double mFramesPerSecond;
   const std::shared_ptr<std::vector<Sample>> mHistory;
   std::optional<int> mLastFrameCounter;
};

class DynamicRangeProcessorHistory;
using DynamicRangeProcessorHistoryUPtr =
   std::unique_ptr<DynamicRangeProcessorHistory>;
