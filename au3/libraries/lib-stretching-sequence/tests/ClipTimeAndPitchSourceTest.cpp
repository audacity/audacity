/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipTimeAndPitchSourceTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipTimeAndPitchSource.h"
#include "AudioContainer.h"
#include "FloatVectorClip.h"

#include <catch2/catch.hpp>

namespace {
constexpr auto sampleRate = 3;
using FloatVectorVector = std::vector<std::vector<float> >;
} // namespace

TEST_CASE("ClipTimeAndPitchSource")
{
    const auto direction
        =GENERATE(PlaybackDirection::forward, PlaybackDirection::backward);

    SECTION("Pull reads correctly past the end of the clip")
    {
        const auto clip = std::make_shared<FloatVectorClip>(
            sampleRate, FloatVectorVector { { 1.f, 2.f, 3.f, 4.f, 5.f } });
        ClipTimeAndPitchSource sut { *clip, 0., direction };
        constexpr auto bufferSize = 3;
        AudioContainer output(bufferSize, 1u);

        sut.Pull(output.channelPointers.data(), bufferSize);
        const auto firstExpected = direction == PlaybackDirection::forward
                                   ? std::vector<float> { 1.f, 2.f, 3.f }
        : std::vector<float> { 5.f, 4.f, 3.f };
        REQUIRE(output.channelVectors[0] == firstExpected);

        sut.Pull(output.channelPointers.data(), bufferSize);
        const auto secondExpected = direction == PlaybackDirection::forward
                                    ? std::vector<float> { 4.f, 5.f, 0.f }
        : std::vector<float> { 2.f, 1.f, 0.f };
        REQUIRE(output.channelVectors[0] == secondExpected);
    }
}
