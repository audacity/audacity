/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentSampleViewTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioSegmentSampleView.h"

#include <catch2/catch.hpp>

TEST_CASE("AudioSegmentSampleView", "Copy returns expected values when")
{
    SECTION("AudioSegmentSampleView is silent")
    {
        AudioSegmentSampleView sut { 3 };
        std::vector<float> buffer(4);
        std::fill(buffer.begin(), buffer.end(), 1.f);
        SECTION("and asked MORE values than it holds.")
        {
            sut.Copy(buffer.data(), 4u);
            REQUIRE(buffer == std::vector<float> { 0.f, 0.f, 0.f, 0.f });
        }
        SECTION("and asked FEWER values than it holds.")
        {
            sut.Copy(buffer.data(), 2u);
            REQUIRE(buffer == std::vector<float> { 0.f, 0.f, 1.f, 1.f });
        }
    }

    SECTION("AudioSegmentSampleView is NOT silent")
    {
        const auto segment1 = std::make_shared<std::vector<float> >(
            std::vector<float> { 1.f, 2.f, 3.f });
        const auto segment2 = std::make_shared<std::vector<float> >(
            std::vector<float> { 4.f, 5.f, 6.f });
        constexpr auto start = 2u;
        constexpr auto length = 3u;
        AudioSegmentSampleView sut { { segment1, segment2 }, start, length };

        SECTION("and asked MORE values than it holds.")
        {
            std::vector<float> out(4u);
            std::fill(out.begin(), out.end(), 123.f);
            sut.Copy(out.data(), out.size());
            REQUIRE(out == std::vector<float> { 3.f, 4.f, 5.f, 0.f });
        }
        SECTION("and asked FEWER values than it holds.")
        {
            std::vector<float> out(2u);
            sut.Copy(out.data(), out.size());
            REQUIRE(out == std::vector<float> { 3.f, 4.f });
        }
    }
}
