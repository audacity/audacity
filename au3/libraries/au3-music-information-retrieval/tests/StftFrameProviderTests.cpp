/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StftFrameProviderTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MirTypes.h"
#include "MirUtils.h"
#include "StftFrameProvider.h"

#include <catch2/catch.hpp>

namespace MIR {
namespace {
class TestMirAudioReader : public MirAudioReader
{
public:
    const long long numSamples;

    TestMirAudioReader(long long numSamples)
        : numSamples{numSamples}
    {
    }

    double GetSampleRate() const override
    {
        return 44100;
    }

    long long GetNumSamples() const override
    {
        return numSamples;
    }

    void
    ReadFloats(float* buffer, long long where, size_t numFrames) const override
    {
        REQUIRE(where >= 0);
        REQUIRE(where + numFrames <= numSamples);
    }
};
} // namespace
TEST_CASE("StftFrameProvider")
{
    SECTION("handles empty files")
    {
        StftFrameProvider sut { TestMirAudioReader { 0 } };
        PffftFloatVector frame;
        REQUIRE(!sut.GetNextFrame(frame));
    }
    SECTION("handles very short files")
    {
        StftFrameProvider sut { TestMirAudioReader { 1 } };
        PffftFloatVector frame;
        REQUIRE(!sut.GetNextFrame(frame));
    }
    SECTION("has power-of-two number of frames")
    {
        StftFrameProvider sut { TestMirAudioReader { 123456 } };
        REQUIRE(IsPowOfTwo(sut.GetNumFrames()));
    }
    SECTION("respects MirAudioReader boundaries")
    {
        TestMirAudioReader reader { 123456 };
        StftFrameProvider sut { reader };
        PffftFloatVector frame;
        while (sut.GetNextFrame(frame)) {
        }
    }
}
} // namespace MIR
