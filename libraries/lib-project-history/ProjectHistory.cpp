/**********************************************************************

Audacity: A Digital Audio Editor

ProjectHistory.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#include "ProjectHistory.h"

#include "Project.h"
#include "Track.h"
#include "UndoManager.h"
#include "ViewInfo.h"

static AudacityProject::AttachedObjects::RegisteredFactory sProjectHistoryKey {
   []( AudacityProject &project ) {
      return std::make_shared< ProjectHistory >( project );
   }
};

ProjectHistory &ProjectHistory::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectHistory >( sProjectHistoryKey );
}

const ProjectHistory &ProjectHistory::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectHistory::~ProjectHistory() = default;

//
// Undo/History methods
//

void ProjectHistory::InitialState()
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );

   undoManager.ClearStates();

   undoManager.PushState(
      tracks, viewInfo.selectedRegion,
      XO("Created new project"), {});

   undoManager.StateSaved();
}

bool ProjectHistory::UndoAvailable() const
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &undoManager = UndoManager::Get( project );
   return undoManager.UndoAvailable() &&
       !tracks.HasPendingTracks();
}

bool ProjectHistory::RedoAvailable() const
{
   auto &project = mProject;
   auto &tracks = TrackList::Get( project );
   auto &undoManager = UndoManager::Get( project );
   return undoManager.RedoAvailable() &&
      !tracks.HasPendingTracks();
}

void ProjectHistory::PushState(
   const TranslatableString &desc, const TranslatableString &shortDesc)
{
   PushState(desc, shortDesc, UndoPush::NONE);
}

void ProjectHistory::PushState(const TranslatableString &desc,
                                const TranslatableString &shortDesc,
                                UndoPush flags )
{
   auto &project = mProject;
   if((flags & UndoPush::NOAUTOSAVE) == UndoPush::NONE)
      AutoSave::Call(project);

   // remaining no-fail operations "commit" the changes of undo manager state
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   undoManager.PushState(
      tracks, viewInfo.selectedRegion,
      desc, shortDesc, flags);

   mDirty = true;
}

void ProjectHistory::RollbackState()
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get( project );
   SetStateTo( undoManager.GetCurrentState(), false );
}

void ProjectHistory::ModifyState(bool bWantsAutoSave)
{
   auto &project = mProject;
   if (bWantsAutoSave)
      AutoSave::Call(project);

   // remaining no-fail operations "commit" the changes of undo manager state
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   undoManager.ModifyState(tracks, viewInfo.selectedRegion);
}

// LL:  Is there a memory leak here as "l" and "t" are not deleted???
// Vaughan, 2010-08-29: No, as "l" is a TrackList* of an Undo stack state.
//    Need to keep it and its tracks "t" available for Undo/Redo/SetStateTo.
void ProjectHistory::PopState(const UndoState &state, bool doAutosave)
{
   auto &project = mProject;
   if (doAutosave)
      AutoSave::Call(project);

   // remaining no-fail operations "commit" the changes of undo manager state
   TrackList *const tracks = state.tracks.get();
   wxASSERT(tracks);
   auto &dstTracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   viewInfo.selectedRegion = state.selectedRegion;

   // Restore extra state
   for (auto &pExtension : state.extensions)
      if (pExtension)
         pExtension->RestoreUndoRedoState(project);

   dstTracks.Clear();
   for (auto t : tracks->Any())
      dstTracks.Add(t->Duplicate());
}

void ProjectHistory::SetStateTo(unsigned int n, bool doAutosave)
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get( project );

   undoManager.SetStateTo(n,
      [this, doAutosave]( const UndoStackElem &elem ){
         PopState(elem.state, doAutosave); } );
}
