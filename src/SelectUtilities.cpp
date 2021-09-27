/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 SelectUtilities.cpp
 
 Paul Licameli split from SelectMenus.cpp
 
 **********************************************************************/

#include "SelectUtilities.h"

#include <wx/frame.h>

#include "widgets/AudacityMessageBox.h"
#include "CommonCommandFlags.h"
#include "Menus.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "ProjectSettings.h"
#include "SelectionState.h"
#include "TrackPanelAx.h"
#include "TrackPanel.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

namespace {

// Temporal selection (not TimeTrack selection)
// potentially for all wave tracks.
void DoSelectTimeAndAudioTracks
(AudacityProject &project, bool bAllTime, bool bAllTracks)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   if( bAllTime )
      selectedRegion.setTimes(
         tracks.GetMinOffset(), tracks.GetEndTime());

   if( bAllTracks ) {
      // Unselect all tracks before selecting audio.
      for (auto t : tracks.Any())
         t->SetSelected(false);
      for (auto t : tracks.Any<WaveTrack>())
         t->SetSelected(true);

      ProjectHistory::Get( project ).ModifyState(false);
   }
}

}
namespace SelectUtilities {

void DoSelectTimeAndTracks
(AudacityProject &project, bool bAllTime, bool bAllTracks)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   if( bAllTime )
      selectedRegion.setTimes(
         tracks.GetMinOffset(), tracks.GetEndTime());

   if( bAllTracks ) {
      for (auto t : tracks.Any())
         t->SetSelected(true);

      ProjectHistory::Get( project ).ModifyState(false);
   }
}

void SelectNone( AudacityProject &project )
{
   auto &tracks = TrackList::Get( project );
   for (auto t : tracks.Any())
      t->SetSelected(false);

   auto &trackPanel = TrackPanel::Get( project );
   trackPanel.Refresh(false);
}

// Select the full time range, if no
// time range is selected.
void SelectAllIfNone( AudacityProject &project )
{
   auto &viewInfo = ViewInfo::Get( project );
   auto flags = MenuManager::Get( project ).GetUpdateFlags();
   if((flags & EditableTracksSelectedFlag()).none() ||
      viewInfo.selectedRegion.isPoint())
      DoSelectAllAudio( project );
}

// Select the full time range, if no time range is selected and
// selecting is allowed. Returns "false" selecting not allowed.
bool SelectAllIfNoneAndAllowed( AudacityProject &project )
{
   auto allowed = gPrefs->ReadBool(wxT("/GUI/SelectAllOnNone"), false);
   auto &viewInfo = ViewInfo::Get( project );
   auto flags = MenuManager::Get( project ).GetUpdateFlags();

   if((flags & EditableTracksSelectedFlag()).none() ||
      viewInfo.selectedRegion.isPoint()) {
      if (!allowed) {
         return false;
      }
      DoSelectAllAudio( project );
   }
   return true;
}

void DoListSelection
(AudacityProject &project, Track *t, bool shift, bool ctrl, bool modifyState)
{
   auto &tracks = TrackList::Get( project );
   auto &selectionState = SelectionState::Get( project );
   const auto &settings = ProjectSettings::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = GetProjectFrame( project );

   auto isSyncLocked = settings.IsSyncLocked();

   selectionState.HandleListSelection(
      tracks, viewInfo, *t,
      shift, ctrl, isSyncLocked );

   if (! ctrl )
      TrackFocus::Get( project ).Set( t );
   window.Refresh(false);
   if (modifyState)
      ProjectHistory::Get( project ).ModifyState(true);
}

void DoSelectAll(AudacityProject &project)
{
   DoSelectTimeAndTracks( project, true, true );
}

void DoSelectAllAudio(AudacityProject &project)
{
   DoSelectTimeAndAudioTracks( project, true, true );
}

// This function selects all tracks if no tracks selected,
// and all time if no time selected.
// There is an argument for making it just count wave tracks,
// However you could then not select a label and cut it,
// without this function selecting all tracks.
void DoSelectSomething(AudacityProject &project)
{
   auto &tracks = TrackList::Get( project );
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   bool bTime = selectedRegion.isPoint();
   bool bTracks = tracks.Selected().empty();

   if( bTime || bTracks )
      DoSelectTimeAndTracks( project, bTime, bTracks );
}

void LockPlayRegion(AudacityProject &project)
{
   auto &tracks = TrackList::Get( project );

   auto &viewInfo = ViewInfo::Get( project );
   auto &playRegion = viewInfo.playRegion;
   if (playRegion.GetStart() >= tracks.GetEndTime()) {
      AudacityMessageBox(
         XO("Cannot lock region beyond\nend of project."),
         XO("Error"));
   }
   else {
      playRegion.SetLocked( true );
   }
}

void UnlockPlayRegion(AudacityProject &project)
{
   auto &viewInfo = ViewInfo::Get( project );
   auto &playRegion = viewInfo.playRegion;
   playRegion.SetLocked( false );
}

}
