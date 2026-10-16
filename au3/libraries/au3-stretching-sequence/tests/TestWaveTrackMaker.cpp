/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveTrackMaker.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "TestWaveTrackMaker.h"
#include "MockedAudio.h"
#include "MockedPrefs.h"
#include "au3-project/Project.h"

#define CATCH_CONFIG_EXTERNAL_INTERFACES
#include <catch2/catch.hpp>

namespace {
// Shared by every test case of this executable. Created before the first test runs rather than at
// static-initialization time: MockedPrefs runs InitPreferences, which reaches statics of BasicUI
// whose construction order relative to this object is unspecified (they are all in one executable
// now that the AU3 libraries are static).
struct TestEnvironment
{
    MockedPrefs prefs;
    MockedAudio audio;
    decltype(AudacityProject::Create()) project = AudacityProject::Create();
    decltype(TrackList::Create(nullptr)) tracks = TrackList::Create(project.get());
};

TestEnvironment& Environment()
{
    static TestEnvironment environment;
    return environment;
}

struct TestEnvironmentListener : Catch::TestEventListenerBase
{
    using TestEventListenerBase::TestEventListenerBase;
    void testRunStarting(const Catch::TestRunInfo&) override { Environment(); }
};
CATCH_REGISTER_LISTENER(TestEnvironmentListener)
}

TestWaveTrackMaker::TestWaveTrackMaker(
    int sampleRate, SampleBlockFactoryPtr factory)
    : mSampleRate{sampleRate}
    , mFactory{factory}
{
}

std::shared_ptr<WaveTrack>
TestWaveTrackMaker::Track(const WaveClipHolders& clips) const
{
    const auto track = WaveTrack::Create(
        mFactory, floatSample, mSampleRate);
    Environment().tracks->Add(track);
    for (const auto& clip : clips) {
        track->InsertInterval(clip, true);
    }
    return track;
}

std::shared_ptr<WaveTrack>
TestWaveTrackMaker::Track(const WaveClipHolder& clip) const
{
    return Track(WaveClipHolders { clip });
}
