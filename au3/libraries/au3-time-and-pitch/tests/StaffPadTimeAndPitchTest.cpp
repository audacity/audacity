/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StaffPadTimeAndPitchTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "StaffPadTimeAndPitch.h"
#include "AudioContainer.h"
#include "MockedPrefs.h"
#include "TimeAndPitchFakeSource.h"
#include "TimeAndPitchRealSource.h"
#include "WavFileIO.h"

#include <catch2/catch.hpp>

using namespace std::literals::string_literals;
using namespace std::literals::chrono_literals;

TEST_CASE("StaffPadTimeAndPitch")
{
    MockedPrefs mockedPrefs;
    SECTION("Smoke test")
    {
        using TestParameter
            =std::pair<std::string, std::optional<std::chrono::seconds> >;
        const auto [filenameStem, upTo] = GENERATE(
            TestParameter { "AudacitySpectral", std::nullopt },
            TestParameter { "FifeAndDrumsStereo", 3s });
        const auto inputPath = std::string(CMAKE_SOURCE_DIR) + "/tests/samples/"
                               + filenameStem + ".wav";

        std::optional<std::string> outputDir;
        // Uncomment this if you want to look at the output locally.
        // outputDir = "C:/Users/saint/Downloads/StaffPadTimeAndPitchTestOut";

        std::vector<std::vector<float> > input;
        AudioFileInfo info;
        REQUIRE(WavFileIO::Read(inputPath, input, info, upTo));
        for (const auto pitchRatio : std::vector<std::pair<int, int> > {
            { 4, 5 },   // major 3rd down
            { 1, 1 },   // no shift
            { 5, 4 },   // major 3rd up
        }) {
            const auto pr
                =static_cast<double>(pitchRatio.first) / pitchRatio.second;
            for (const auto timeRatio : std::vector<std::pair<int, int> > {
                { 1, 2 },
                { 1, 1 },
                { 2, 1 },
            }) {
                const auto tr
                    =static_cast<double>(timeRatio.first) / timeRatio.second;
                const auto numOutputFrames
                    =static_cast<size_t>(info.numFrames * tr);
                AudioContainer container(numOutputFrames, info.numChannels);
                TimeAndPitchInterface::Parameters params;
                params.timeRatio = tr;
                params.pitchRatio = pr;
                TimeAndPitchRealSource src(input);
                StaffPadTimeAndPitch sut(
                    info.sampleRate, info.numChannels, src, std::move(params));
                constexpr size_t blockSize = 1234u;
                auto offset = 0u;
                while (offset < numOutputFrames)
                {
                    std::vector<float*> offsetBuffers(info.numChannels);
                    for (auto i = 0u; i < info.numChannels; ++i) {
                        offsetBuffers[i] = container.channelPointers[i] + offset;
                    }
                    const auto numToRead
                        =std::min(numOutputFrames - offset, blockSize);
                    sut.GetSamples(offsetBuffers.data(), numToRead);
                    offset += numToRead;
                }

                if (outputDir) {
                    const auto filename = filenameStem + "_t"s
                                          + std::to_string(timeRatio.first) + "-"
                                          + std::to_string(timeRatio.second) + "_p"
                                          + std::to_string(pitchRatio.first) + "-"
                                          + std::to_string(pitchRatio.second) + ".wav";
                    const auto outputPath = *outputDir + "/" + filename;
                    REQUIRE(WavFileIO::Write(
                                outputPath, container.channelVectors, info.sampleRate));
                }
            }
        }
    }

    SECTION("Not specifying time or pitch or ratio yields bit-exact results")
    {
        // Although this is no hard-proof, a bit-exact output is a strong evidence
        // that no processing happens if no time or pitch ratio is specified,
        // which is what we want.

        const auto inputPath
            =std::string(CMAKE_SOURCE_DIR) + "/tests/samples/AudacitySpectral.wav";
        std::vector<std::vector<float> > input;
        AudioFileInfo info;
        REQUIRE(WavFileIO::Read(inputPath, input, info));
        AudioContainer container(info.numFrames, info.numChannels);
        TimeAndPitchInterface::Parameters params;
        TimeAndPitchRealSource src(input);
        StaffPadTimeAndPitch sut(
            info.sampleRate, info.numChannels, src, std::move(params));
        sut.GetSamples(container.Get(), info.numFrames);
        // Calling REQUIRE(container.channelVectors == input) directly for large
        // vectors might be rough on stdout in case of an error printout ...
        const auto outputEqualsInput = container.channelVectors == input;
        REQUIRE(outputEqualsInput);
    }

    SECTION("Extreme stretch ratios")
    {
        constexpr auto originalDuration = 60.;    // 1 minute
        constexpr auto targetDuration = 1. / 44100; // 1 sample
        constexpr auto superSmallRatio = targetDuration / originalDuration;
        constexpr auto requestedNumSamples
            =1; // ... a single sample, but which is the condensed form of one
                // minute of audio :D
        constexpr auto numChannels = 1;
        TimeAndPitchFakeSource src;
        AudioContainer container(requestedNumSamples, numChannels);
        TimeAndPitchInterface::Parameters params;
        params.timeRatio = superSmallRatio;
        StaffPadTimeAndPitch sut(44100, numChannels, src, std::move(params));
        sut.GetSamples(
            container.Get(),
            requestedNumSamples); // This is just not supposed to hang.
    }
}
