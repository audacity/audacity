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

struct Handler : CommandHandlerObject {
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
   left->SetName(tracks.MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
   tracks.Add(left);
   left->SetSelected(true);

   auto right = trackFactory.Create(defaultFormat, rate);
   right->SetName(left->GetName());
   tracks.Add(right);
   right->SetSelected(true);

   tracks.MakeMultiChannelTrack(*left, 2, true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new stereo audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(left.get());
   left->EnsureVisible();
}

};

CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static Handler instance;
   return instance;
}

#define FN(X) (&Handler :: X)
AttachedItem sAttachment{ wxT("Tracks/Add/Add"),
   ( FinderScope{ findCommandHandler },
   Items( "",
      Command( wxT("NewMonoTrack"), XXO("&Mono Track"), FN(OnNewWaveTrack),
         AudioIONotBusyFlag(), wxT("Ctrl+Shift+N") ),
      Command( wxT("NewStereoTrack"), XXO("&Stereo Track"),
         FN(OnNewStereoTrack), AudioIONotBusyFlag() )
   ) )
};
#undef FN

}
