/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StretchingSequenceIntegrationTest.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioContainerHelper.h"
#include "MockPlayableSequence.h"
#include "MockSampleBlockFactory.h"
#include "SampleFormat.h"
#include "StretchingSequence.h"
#include "TestWaveClipMaker.h"
#include "TestWaveTrackMaker.h"

#include <catch2/catch.hpp>

namespace {
constexpr auto sampleRate = 3;
constexpr auto iChannel = 0u;

const auto sampleBlockFactory = std::make_shared<MockSampleBlockFactory>();
TestWaveClipMaker clipMaker { sampleRate, sampleBlockFactory };
TestWaveTrackMaker trackMaker { sampleRate, sampleBlockFactory };
} // namespace

TEST_CASE("StretchingSequence integration tests")
{
    const auto numChannels
        =GENERATE(1 /*, 2*/); // When wide WaveTrack is implemented this test
                              // should also run in stereo.
    const auto mockSequence
        =std::make_shared<MockPlayableSequence>(sampleRate, numChannels);

    SECTION(
        "StretchingSequence and WaveTrack have identical outputs when clips aren't stretched.")
    {
        constexpr auto totalLength
            =sampleRate * 4; // 1s no clip, 1s minusClip, 1s no clip, 1s plusClip

        const auto minusClip = clipMaker.ClipFilledWith(
            -.5f, sampleRate, numChannels,
            [](auto& clip) { clip.SetPlayStartTime(1.0); });
        const auto plusClip = clipMaker.ClipFilledWith(
            +.5f, sampleRate, numChannels,
            [](auto& clip) { clip.SetPlayStartTime(3.0); });

        const auto sut = StretchingSequence::Create(
            *mockSequence, ClipConstHolders { minusClip, plusClip });
        const auto track = trackMaker.Track({ minusClip, plusClip });
        constexpr auto backwards = false;
        AudioContainer sutOutput(totalLength, numChannels);
        REQUIRE(sut->DoGet(
                    iChannel, numChannels,
                    AudioContainerHelper::GetData<char>(sutOutput).data(), floatSample, 0u,
                    totalLength, backwards));

        AudioContainer waveTrackOutput(totalLength, numChannels);
        REQUIRE(track->DoGet(
                    iChannel, numChannels,
                    AudioContainerHelper::GetData<char>(waveTrackOutput).data(),
                    floatSample, 0u, totalLength, backwards));

        const auto outputsAreIdentical
            =sutOutput.channelVectors == waveTrackOutput.channelVectors;
        REQUIRE(outputsAreIdentical);
    }
}
