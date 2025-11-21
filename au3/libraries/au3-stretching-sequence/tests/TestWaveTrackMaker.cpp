/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TestWaveTrackMaker.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "TestWaveTrackMaker.h"
#include "MockedAudio.h"
#include "MockedPrefs.h"
#include "Project.h"

MockedPrefs prefs;
MockedAudio audio;
const auto project = AudacityProject::Create();
const auto tracks = TrackList::Create(project.get());

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
    tracks->Add(track);
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
