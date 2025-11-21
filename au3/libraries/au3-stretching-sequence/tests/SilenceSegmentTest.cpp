/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SilenceSegmentTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "SilenceSegment.h"
#include "AudioContainer.h"

#include <catch2/catch.hpp>

TEST_CASE("SilenceSegment", "behaves as expected")
{
    // A quick test of an easy implementation.
    const auto numChannels = GENERATE(1, 2);
    constexpr auto silenceSegmentLength = 3u;
    SilenceSegment sut(numChannels, silenceSegmentLength);
    REQUIRE(!sut.Empty());
    AudioContainer container(3u, numChannels);
    REQUIRE(sut.GetFloats(container.channelPointers.data(), 1) == 1);
    REQUIRE(!sut.Empty());
    REQUIRE(sut.GetFloats(container.channelPointers.data(), 3) == 2);
    REQUIRE(sut.Empty());
}
