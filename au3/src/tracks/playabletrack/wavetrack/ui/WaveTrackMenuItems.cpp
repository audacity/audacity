/**********************************************************************

Audacity: A Digital Audio Editor

@file WaveTrackMenuItems.cpp
@brief Injects menu items using WaveTrack but not the views of it

Paul Licameli split from TrackMenus.cpp

**********************************************************************/

#include "CommonCommandFlags.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"

#include "SelectUtilities.h"
#include "TrackFocus.h"
#include "Viewport.h"
#include "WaveTrack.h"
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "QualitySettings.h"

namespace {
using namespace MenuRegistry;

void OnNewWaveTrack(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& trackFactory = WaveTrackFactory::Get(project);

    auto defaultFormat = QualitySettings::SampleFormatChoice();

    auto rate = ProjectRate::Get(project).GetRate();

    auto track = trackFactory.Create(defaultFormat, rate);
    track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
    tracks.Add(track);
    SelectUtilities::SelectNone(project);

    track->SetSelected(true);

    ProjectHistory::Get(project)
    .PushState(XO("Created new audio track"), XO("New Track"));

    TrackFocus::Get(project).Set(track.get());
    Viewport::Get(project).ShowTrack(*track);
}

void OnNewStereoTrack(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& trackFactory = WaveTrackFactory::Get(project);

    auto defaultFormat = QualitySettings::SampleFormatChoice();
    auto rate = ProjectRate::Get(project).GetRate();

    SelectUtilities::SelectNone(project);

    tracks.Add(trackFactory.Create(2, defaultFormat, rate));
    auto& newTrack = **tracks.rbegin();
    newTrack.SetSelected(true);
    newTrack.SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));

    ProjectHistory::Get(project)
    .PushState(XO("Created new stereo audio track"), XO("New Track"));

    TrackFocus::Get(project).Set(&newTrack);
    Viewport::Get(project).ShowTrack(newTrack);
}

AttachedItem sAttachment{
    Items("",
          Command(wxT("NewMonoTrack"), XXO("&Mono Track"), OnNewWaveTrack,
                  AudioIONotBusyFlag(), wxT("Ctrl+Shift+N")),
          Command(wxT("NewStereoTrack"), XXO("&Stereo Track"),
                  OnNewStereoTrack, AudioIONotBusyFlag())
          ),
    wxT("Tracks/Add/Add")
};
}
