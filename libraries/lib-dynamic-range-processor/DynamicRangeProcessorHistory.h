// TODO header
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include <memory>
#include <optional>
#include <vector>

class DYNAMIC_RANGE_PROCESSOR_API DynamicRangeProcessorHistory final
{
public:
   DynamicRangeProcessorHistory(double sampleRate);

   struct Packet
   {
      float time = 0.f;
      float target = 0.f;
      float follower = 0.f;
   };

   static constexpr auto maxTimeSeconds = 10.f;

   using Segment = std::vector<Packet>;

   void Push(const std::vector<DynamicRangeProcessorOutputPacket>& packets);
   void BeginNewSegment();
   const std::vector<Segment>& GetSegments() const;

private:
   float GetPacketTime(const DynamicRangeProcessorOutputPacket& packet) const;

   const double mSampleRate;
   bool mBeginNewSegment = true;
   std::vector<Segment> mSegments;
   std::optional<long long> mFirstPacketFirstSampleIndex;
};

class DynamicRangeProcessorHistory;
