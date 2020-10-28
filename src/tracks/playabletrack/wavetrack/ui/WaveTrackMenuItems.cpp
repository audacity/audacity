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

   auto t = tracks.Add( trackFactory.NewWaveTrack( defaultFormat, rate ) );
   SelectUtilities::SelectNone( project );

   t->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(t);
   t->EnsureVisible();
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

   auto left = tracks.Add( trackFactory.NewWaveTrack( defaultFormat, rate ) );
   left->SetSelected(true);

   auto right = tracks.Add( trackFactory.NewWaveTrack( defaultFormat, rate ) );
   right->SetSelected(true);

   tracks.MakeMultiChannelTrack(*left, 2, true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new stereo audio track"), XO("New Track"));

   TrackFocus::Get(project).Set(left);
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
