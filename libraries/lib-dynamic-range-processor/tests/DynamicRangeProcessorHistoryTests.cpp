// TODO header
#include "DynamicRangeProcessorHistory.h"
#include "DynamicRangeProcessorTypes.h"
#include <catch2/catch.hpp>

TEST_CASE("DynamicRangeProcessorHistory")
{
   using SUT = DynamicRangeProcessorHistory;
   constexpr auto numFramesInHistory = 10;
   constexpr int framesPerSecond = numFramesInHistory / SUT::maxTimeSeconds;
   SUT sut { framesPerSecond };
   const auto& segments = sut.GetSegments();
   REQUIRE(segments.empty());

   // Pushing two samples
   sut.Push({ { 1 }, { 2 } });
   REQUIRE(!segments.empty());

   const auto& history = segments.front();
   REQUIRE(history.size() == 2);

   // Pushing one new sample, and two old samples are also pushed
   // redundantly.
   sut.Push({ { 1 }, { 2 }, { 3 } });
   REQUIRE(history.size() == 3);

   // Pushing two fresh samples
   sut.Push({ { 4 }, { 5 } });
   REQUIRE(history.size() == 5);

   SECTION("Discards outdates samples")
   {
      // Big drop-out, data was lost.
      sut.Push({ { 20 } });
      REQUIRE(history.size() == 1);
   }
}
