/**********************************************************************

Audacity: A Digital Audio Editor

@file WaveTrackMenuItems.cpp
@brief Injects menu items using WaveTrack but not the views of it

Paul Licameli split from TrackMenus.cpp

**********************************************************************/

#include "CommonCommandFlags.h"
#include "ProjectHistory.h"
#include "ProjectRate.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "TrackPanelAx.h"
#include "WaveTrack.h"
#include "../../../../commands/CommandContext.h"
#include "../../../../commands/CommandManager.h"
#include "QualitySettings.h"

namespace {
using namespace MenuTable;

void OnNewWaveTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &window = ProjectWindow::Get( project );

   auto defaultFormat = QualitySettings::SampleFormatChoice();

   auto rate = ProjectRate::Get(project).GetRate();

   auto track = trackFactory.Create(defaultFormat, rate);
   track->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
   tracks.Add(track);
   SelectUtilities::SelectNone( project );

   track->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(track.get());
   track->EnsureVisible();
}

void OnNewStereoTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &window = ProjectWindow::Get( project );

   auto defaultFormat = QualitySettings::SampleFormatChoice();
   auto rate = ProjectRate::Get(project).GetRate();

   SelectUtilities::SelectNone( project );

   auto left = trackFactory.Create(defaultFormat, rate);
   tracks.Add(left);

   auto right = trackFactory.Create(defaultFormat, rate);
   tracks.Add(right);

   tracks.MakeMultiChannelTrack(*left, 2, true);

   left->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
   left->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new stereo audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(left.get());
   left->EnsureVisible();
}

AttachedItem sAttachment{ wxT("Tracks/Add/Add"),
   Items( "",
      Command( wxT("NewMonoTrack"), XXO("&Mono Track"), OnNewWaveTrack,
         AudioIONotBusyFlag(), wxT("Ctrl+Shift+N") ),
      Command( wxT("NewStereoTrack"), XXO("&Stereo Track"),
         OnNewStereoTrack, AudioIONotBusyFlag() )
   )
};

}
