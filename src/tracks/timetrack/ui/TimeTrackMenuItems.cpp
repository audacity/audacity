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
#include "../../../commands/CommandContext.h"
#include "../../../commands/CommandManager.h"
#include "AudacityMessageBox.h"

namespace {
using namespace MenuTable;

void OnNewTimeTrack(const CommandContext &context)
{
   auto &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &window = ProjectWindow::Get( project );
   

   if ( *tracks.Any<TimeTrack>().begin() ) {
      AudacityMessageBox(
         XO(
"This version of Audacity only allows one time track for each project window.") );
      return;
   }

   auto t = tracks.AddToHead(std::make_shared<TimeTrack>());

   SelectUtilities::SelectNone( project );

   t->SetSelected(true);

   ProjectHistory::Get( project )
      .PushState(XO("Created new time track"), XO("New Track"));

   TrackFocus::Get(project).Set(t);
   t->EnsureVisible();
}

AttachedItem sAttachment{ wxT("Tracks/Add/Add"),
     Command( wxT("NewTimeTrack"), XXO("&Time Track"),
        OnNewTimeTrack, AudioIONotBusyFlag()
 )
};

}
