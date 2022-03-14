/**********************************************************************

Audacity: A Digital Audio Editor

@file TimeTrackMenuItems.cpp
@brief Injects menu items using TimeTrack but not the views of it

Paul Licameli split from TrackMenus.cpp

**********************************************************************/

#include "CommonCommandFlags.h"
#include "ProjectHistory.h"
#include "ProjectWindow.h"
#include "SelectUtilities.h"
#include "TimeTrack.h"
#include "TrackPanelAx.h"
#include "ViewInfo.h"
#include "../../../commands/CommandContext.h"
#include "../../../commands/CommandManager.h"
#include "widgets/AudacityMessageBox.h"

namespace {
using namespace MenuTable;

struct Handler : CommandHandlerObject {
void OnNewTimeTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   if ( *tracks.Any<TimeTrack>().begin() ) {
      AudacityMessageBox(
         XO(
"This version of Audacity only allows one time track for each project window.") );
      return;
   }

   auto t = tracks.AddToHead( std::make_shared<TimeTrack>(&viewInfo) );

   SelectUtilities::SelectNone( project );

   t->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new time track"), XO("New Track"));

   TrackFocus::Get(project).Set(t);
   t->EnsureVisible();
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
     Command( wxT("NewTimeTrack"), XXO("&Time Track"),
        FN(OnNewTimeTrack), AudioIONotBusyFlag() )
 )
};
#undef FN

}
