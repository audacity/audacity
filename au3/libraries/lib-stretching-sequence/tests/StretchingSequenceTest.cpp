/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StretchingSequenceTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "StretchingSequence.h"
#include "AudioContainer.h"
#include "AudioContainerHelper.h"
#include "FloatVectorClip.h"
#include "MockAudioSegmentFactory.h"
#include "MockPlayableSequence.h"
#include "SampleFormat.h"
#include "WavFileIO.h"

#include <array>
#include <catch2/catch.hpp>

using namespace std::literals::string_literals;
using namespace std::literals::chrono_literals;

namespace {
constexpr auto sampleRate = 3;
} // namespace

TEST_CASE("StretchingSequence unit tests")
{
    SECTION("Get")
    {
        constexpr auto backwards = true;
        constexpr auto numChannels = 1u;

        SECTION("Samples can be queried backwards")
        {
            // Mimic the way a client would use the legacy API to read samples
            // backwards.
            // Our sequence : 1s silence, 1s clip, 1s silence, 1s clip.
            const auto clip1 = std::make_shared<FloatVectorClip>(
                sampleRate, std::vector<float> { 1.f, 2.f, 3.f }, numChannels);
            const auto clip3 = std::make_shared<FloatVectorClip>(
                sampleRate, std::vector<float> { 4.f, 5.f, 6.f }, numChannels);
            clip1->playStartTime = 1.0;
            clip3->playStartTime = 3.0;
            const auto mockSequence
                =std::make_shared<MockPlayableSequence>(sampleRate, numChannels);
            const auto sut = StretchingSequence::Create(
                *mockSequence, ClipConstHolders { clip1, clip3 });

            constexpr auto len = 2;
            const std::array<std::vector<float>, 6> expected { { { 6.f, 5.f },
                { 4.f, 0.f },
                { 0.f, 0.f },
                { 3.f, 2.f },
                { 1.f, 0.f },
                { 0.f, 0.f } } };
            auto start = 12;
            for (const auto& expected : expected) {
                AudioContainer output(len, numChannels);
                constexpr auto backwards = true;
                sut->GetFloats(
                    AudioContainerHelper::GetData(output).data(), start, len,
                    backwards);
                REQUIRE(output.channelVectors[0] == expected);
                start -= 2;
            }
        }

        SECTION("reconstructs segment sequence when expected")
        {
            auto factory = new MockAudioSegmentFactory;
            const auto mockSequence
                =std::make_shared<MockPlayableSequence>(sampleRate, numChannels);
            StretchingSequence sut(
                *mockSequence, sampleRate, numChannels,
                std::unique_ptr<AudioSegmentFactoryInterface> { factory });
            constexpr auto numSamples = 3;
            constexpr auto numChannels = 1;
            constexpr auto start = 10; // some random place in the future
            AudioContainer buffer(numSamples, 1);
            REQUIRE(sut.GetFloats(
                        AudioContainerHelper::GetData(buffer).data(), start, numSamples,
                        !backwards));
            // We don't really care about the call count before that.
            const auto refCount = factory->callCount;
            // Expected query: no factory call
            REQUIRE(sut.GetFloats(
                        AudioContainerHelper::GetData(buffer).data(), start + numSamples,
                        numSamples, !backwards));
            REQUIRE(factory->callCount == refCount);
            // Repeat the same query: expect a factory call.
            REQUIRE(sut.GetFloats(
                        AudioContainerHelper::GetData(buffer).data(), start + numSamples,
                        numSamples, !backwards));
            REQUIRE(factory->callCount == refCount + 1);
            // Use expected index, but opposite direction: expect a factory call.
            REQUIRE(sut.GetFloats(
                        AudioContainerHelper::GetData(buffer).data(),
                        start + numSamples * 2, numSamples, backwards));
            REQUIRE(factory->callCount == refCount + 2);
            // Now that we're calling backwards, call with expected index: no
            // factory call.
            REQUIRE(sut.GetFloats(
                        AudioContainerHelper::GetData(buffer).data(), start + numSamples,
                        numSamples, backwards));
            REQUIRE(factory->callCount == refCount + 2);
        }
    }
}

TEST_CASE("StretchingSequence with real audio")
{
    const auto filenameStem = "FifeAndDrumsStereo"s;
    const auto inputPath
        =std::string(CMAKE_SOURCE_DIR) + "/tests/samples/" + filenameStem + ".wav";

    std::optional<std::string> outputDir;
    // Uncomment this if you want to look at the output locally.
    // outputDir = "C:/Users/saint/Downloads/StaffPadTimeAndPitchTestOut";
    const auto backwards = GENERATE(false, true);

    SECTION("with one clip")
    {
        const auto ratio = GENERATE(0.75, 1.5);
        std::vector<std::vector<float> > input;
        AudioFileInfo info;
        REQUIRE(WavFileIO::Read(inputPath, input, info, 7s));
        const auto clip
            =std::make_shared<FloatVectorClip>(info.sampleRate, input);
        clip->stretchRatio = ratio;
        const auto mockSequence = std::make_shared<MockPlayableSequence>(
            info.sampleRate, info.numChannels);
        const auto sut
            =StretchingSequence::Create(*mockSequence, ClipConstHolders { clip });
        const size_t numOutputSamples = clip->stretchRatio * input[0].size() + .5;
        AudioContainer container(numOutputSamples, input.size());
        sut->GetFloats(
            container.channelPointers.data(), backwards ? numOutputSamples : 0,
            numOutputSamples, backwards);
        if (outputDir) {
            const auto filename = "oneclip_"s + filenameStem
                                  + (backwards ? "_bwd" : "_fwd")
                                  + (ratio < 1. ? "_fast" : "_slow") + ".wav";
            const auto outputPath = *outputDir + "/" + filename;
            REQUIRE(WavFileIO::Write(
                        outputPath, container.channelVectors, info.sampleRate));
        }
    }

    SECTION("with several clips")
    {
        std::vector<std::vector<float> > input;
        AudioFileInfo info;
        REQUIRE(WavFileIO::Read(inputPath, input, info, 7s));
        const auto clip1
            =std::make_shared<FloatVectorClip>(info.sampleRate, input);
        const auto clip2
            =std::make_shared<FloatVectorClip>(info.sampleRate, input);
        clip1->stretchRatio = 0.75;
        clip2->stretchRatio = 1.5;

        // 1s of silence, clip 1, 2s of silence, clip 2.
        clip1->playStartTime = 1.;
        clip2->playStartTime = clip1->GetPlayEndTime() + 2.;
        const auto totalDuration = clip2->GetPlayEndTime();

        const auto mockSequence
            =std::make_shared<MockPlayableSequence>(info.sampleRate, 2u);
        const auto sut = StretchingSequence::Create(
            *mockSequence, ClipConstHolders { clip1, clip2 });
        const size_t numOutputSamples
            =totalDuration * info.sampleRate /*assuming both have same sample rate*/
              + .5;
        AudioContainer container(numOutputSamples, 2u);
        const auto start = backwards ? numOutputSamples : 0u;
        sut->GetFloats(
            container.channelPointers.data(), start, numOutputSamples, backwards);
        if (outputDir) {
            const auto filename = "manyClips_"s + filenameStem
                                  + (backwards ? "_bwd" : "_fwd") + ".wav";
            const auto outputPath = *outputDir + "/" + filename;
            REQUIRE(WavFileIO::Write(
                        outputPath, container.channelVectors, info.sampleRate));
        }
    }
}
