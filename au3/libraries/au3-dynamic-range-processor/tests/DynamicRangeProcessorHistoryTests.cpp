/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorHistoryTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
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

    constexpr auto sampsPerPacket = 1;

    // Pushing two samples
    sut.Push({ { 1, sampsPerPacket }, { 2, sampsPerPacket } });
    REQUIRE(!segments.empty());

    const auto& history = segments.front();
    REQUIRE(history.size() == 2);

    // Pushing one new sample, and two old samples are also pushed
    // redundantly.
    sut.Push(
        { { 1, sampsPerPacket }, { 2, sampsPerPacket }, { 3, sampsPerPacket } });
    REQUIRE(history.size() == 3);

    // Pushing two fresh samples
    sut.Push({ { 4, sampsPerPacket }, { 5, sampsPerPacket } });
    REQUIRE(history.size() == 5);
}
