/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveClipSegmentTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipSegment.h"
#include "AudioContainer.h"
#include "FloatVectorClip.h"

#include <catch2/catch.hpp>

namespace {
constexpr auto sampleRate = 3;
using FloatVectorVector = std::vector<std::vector<float> >;
} // namespace

TEST_CASE("ClipSegment")
{
    const auto direction
        =GENERATE(PlaybackDirection::forward, PlaybackDirection::backward);

    SECTION("simple test case")
    {
        const auto numChannels = GENERATE(1u, 2u);
        const auto clip
            =numChannels == 1u
              ? std::make_shared<FloatVectorClip>(
                  sampleRate, FloatVectorVector { { 1.f, 2.f, 3.f } })
              : std::make_shared<FloatVectorClip>(
                  sampleRate,
                  FloatVectorVector { { 1.f, 2.f, 3.f }, { -1.f, -2.f, -3.f } });
        ClipSegment sut { *clip, 0., direction };
        AudioContainer output(sampleRate, numChannels);
        REQUIRE(sut.GetFloats(output.channelPointers.data(), sampleRate) == sampleRate);
        if (numChannels == 1u) {
            const auto expected = direction == PlaybackDirection::forward
                                  ? std::vector<float> { 1.f, 2.f, 3.f }
            : std::vector<float> { 3.f, 2.f, 1.f };
            const auto asExpected = output.channelVectors[0] == expected;
            REQUIRE(asExpected);
        } else {
            const auto expected
                =direction == PlaybackDirection::forward
                  ? std::vector<std::vector<float> > { { 1.f, 2.f, 3.f },
                { -1.f, -2.f, -3.f } }
            : std::vector<std::vector<float> > { { 3.f, 2.f, 1.f },
                { -3.f, -2.f, -1.f } };
            const auto asExpected = output.channelVectors == expected;
            REQUIRE(asExpected);
        }
    }

    SECTION("accounts for playback offset")
    {
        const auto clip = std::make_shared<FloatVectorClip>(
            sampleRate, FloatVectorVector { { 1.f, 2.f, 3.f, 4.f, 5.f } });
        const auto numSamples = clip->GetVisibleSampleCount().as_size_t(); // 5
        // Offset of two samples, in seconds.
        constexpr auto playbackOffset = 2 / static_cast<double>(sampleRate);
        ClipSegment sut { *clip, playbackOffset, direction };
        AudioContainer output(numSamples, 1u);
        REQUIRE(sut.GetFloats(output.channelPointers.data(), numSamples) == 3);
        const auto expected = direction == PlaybackDirection::forward
                              ? std::vector<float> { 3.f, 4.f, 5.f, 0.f, 0.f }
        : std::vector<float> { 3.f, 2.f, 1.f, 0.f, 0.f };
        REQUIRE(output.channelVectors[0] == expected);
    }
}
