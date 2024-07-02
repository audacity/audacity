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

void OnNewWaveTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );

   auto defaultFormat = QualitySettings::SampleFormatChoice();

   auto rate = ProjectRate::Get(project).GetRate();

   auto track = trackFactory.Create(defaultFormat, rate);
   track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));

   wxString newTrackPlacement = gPrefs->Read(wxT("/Tracks/NewTrackPlacement"), wxT("BottomOfProject"));

   if (newTrackPlacement == wxT("TopOfProject")) {
      tracks.Insert(*tracks.begin(), track);
   } else if (newTrackPlacement == wxT("AboveCurrentTrack")) {
      tracks.Insert(*tracks.Selected().begin(), track);
   } else if (newTrackPlacement == wxT("BelowCurrentTrack")) {
      auto last_selection = *tracks.Selected().rbegin();
      tracks.Insert(tracks.GetNext(*last_selection), track);
   } else {
      tracks.Add(track);
   }

   SelectUtilities::SelectNone( project );

   track->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(track.get());
   Viewport::Get(project).ShowTrack(*track);
}

void OnNewStereoTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );

   auto defaultFormat = QualitySettings::SampleFormatChoice();
   auto rate = ProjectRate::Get(project).GetRate();

   SelectUtilities::SelectNone( project );

   auto track = trackFactory.Create(2, defaultFormat, rate);

   wxString newTrackPlacement = gPrefs->Read(wxT("/Tracks/NewTrackPlacement"), wxT("BottomOfProject"));

   if (newTrackPlacement == wxT("TopOfProject")) {
      tracks.Insert(*tracks.begin(), track);
   } else if (newTrackPlacement == wxT("AboveCurrentTrack")) {
      auto focusedTrack = TrackFocus::Get(project).Get();
      tracks.Insert(focusedTrack, track);
   } else if (newTrackPlacement == wxT("BelowCurrentTrack")) {
      auto focusedTrack = TrackFocus::Get(project).Get();
      tracks.Insert(tracks.GetNext(*focusedTrack), track);
   } else {
      tracks.Add(track);
   }

   track->SetSelected(true);
   track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));

   ProjectHistory::Get( project )
      .PushState(XO("Created new stereo audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(track.get());
   Viewport::Get(project).ShowTrack(*track);
}

AttachedItem sAttachment{
   Items( "",
      Command( wxT("NewMonoTrack"), XXO("&Mono Track"), OnNewWaveTrack,
         AudioIONotBusyFlag(), wxT("Ctrl+Shift+N") ),
      Command( wxT("NewStereoTrack"), XXO("&Stereo Track"),
         OnNewStereoTrack, AudioIONotBusyFlag() )
   ),
   wxT("Tracks/Add/Add")
};

}
