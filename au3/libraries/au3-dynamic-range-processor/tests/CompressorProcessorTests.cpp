/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorProcessorTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "CompressorProcessor.h"
#include <catch2/catch.hpp>

TEST_CASE("GetMaxCompressionDb", "simple test")
{
    DynamicRangeProcessorSettings settings { LimiterSettings {} };
    settings.inCompressionThreshDb = -20;
    settings.outCompressionThreshDb = -10;
    settings.kneeWidthDb = 0;

    // Infinite ratio -> max compression simply is the input compression
    // threshold.
    settings.compressionRatio = std::numeric_limits<float>::infinity();
    REQUIRE(CompressorProcessor::GetMaxCompressionDb(settings) == Approx(20));

    // 2:1 ratio -> half the input compression threshold.
    settings.compressionRatio = 2;
    REQUIRE(CompressorProcessor::GetMaxCompressionDb(settings) == Approx(10));

    // So long as the knee half width is less than the input compression
    // threshold, it shouldn't change the result.
    settings.kneeWidthDb = 40;
    REQUIRE(CompressorProcessor::GetMaxCompressionDb(settings) == Approx(10));

    // But if it's greater, it should.
    settings.kneeWidthDb = 60;
    REQUIRE(CompressorProcessor::GetMaxCompressionDb(settings) > 10);
}

TEST_CASE("CompressorProcessor", "smoke test")
{
    CompressorSettings settings;
    settings.lookaheadMs = 5;
    CompressorProcessor sut { settings };
    constexpr auto sampleRate = 44100;
    constexpr auto numChannels = 2;
    constexpr auto blockSize = 44100;
    constexpr auto signalSize = 2 * blockSize;
    sut.Init(sampleRate, numChannels, blockSize);
    std::vector<std::vector<float> > buffer(numChannels);
    std::vector<float*> pointers(numChannels);
    for (auto i = 0; i < numChannels; ++i) {
        auto& in = buffer[i];
        in.resize(signalSize);
        std::fill(in.begin(), in.begin() + signalSize / 2, 0.0f);
        std::fill(in.begin() + signalSize / 2, in.end(), 1.0f);
    }
    auto progress = 0;
    while (progress < signalSize)
    {
        const auto remaining = signalSize - progress;
        const auto toProcess = std::min(remaining, blockSize);
        std::transform(
            buffer.begin(), buffer.end(), pointers.begin(),
            [progress](std::vector<float>& v) { return v.data() + progress; });
        const auto p = pointers.data();
        sut.Process(p, p, toProcess);
        progress += toProcess;
    }
}
