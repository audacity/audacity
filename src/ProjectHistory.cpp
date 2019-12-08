/**********************************************************************

Audacity: A Digital Audio Editor

ProjectHistory.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#include "ProjectHistory.h"

#include "Project.h"
#include "ProjectFileIO.h"
#include "Tags.h"
#include "Track.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "ondemand/ODComputeSummaryTask.h"
#include "ondemand/ODManager.h"

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
   auto &tags = Tags::Get( project );

   undoManager.ClearStates();

   undoManager.PushState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this(),
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
   PushState(desc, shortDesc, UndoPush::AUTOSAVE);
}

void ProjectHistory::PushState(const TranslatableString &desc,
                                const TranslatableString &shortDesc,
                                UndoPush flags )
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &tags = Tags::Get( project );
   undoManager.PushState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this(),
      desc, shortDesc, flags);

   mDirty = true;

   if((flags & UndoPush::AUTOSAVE) != UndoPush::MINIMAL)
      projectFileIO.AutoSave();
}

void ProjectHistory::RollbackState()
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get( project );
   SetStateTo( undoManager.GetCurrentState() );
}

void ProjectHistory::ModifyState(bool bWantsAutoSave)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &undoManager = UndoManager::Get( project );
   auto &tags = Tags::Get( project );
   undoManager.ModifyState(
      &tracks, viewInfo.selectedRegion, tags.shared_from_this());
   if (bWantsAutoSave)
      projectFileIO.AutoSave();
}

// LL:  Is there a memory leak here as "l" and "t" are not deleted???
// Vaughan, 2010-08-29: No, as "l" is a TrackList* of an Undo stack state.
//    Need to keep it and its tracks "t" available for Undo/Redo/SetStateTo.
void ProjectHistory::PopState(const UndoState &state)
{
   auto &project = mProject;
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &dstTracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   viewInfo.selectedRegion = state.selectedRegion;

   // Restore tags
   Tags::Set( project, state.tags );

   TrackList *const tracks = state.tracks.get();

   dstTracks.Clear();
   bool odUsed = false;
   std::unique_ptr<ODComputeSummaryTask> computeTask;

   for (auto t : tracks->Any())
   {
      auto copyTrack = dstTracks.Add(t->Duplicate());

      //add the track to OD if the manager exists.  later we might do a more rigorous check...
      copyTrack->TypeSwitch( [&](WaveTrack *wt) {
         //if the ODManager hasn't been initialized, there's no chance this track has OD blocks since this
         //is a "Redo" operation.
         //TODO: update this to look like the update loop in OpenFile that handles general purpose ODTasks.
         //BUT, it is too slow to go thru every blockfile and check the odtype, so maybe put a flag in wavetrack
         //that gets unset on OD Completion, (and we could also update the drawing there too.)  The hard part is that
         //we would need to watch every possible way a OD Blockfile could get inserted into a wavetrack and change the
         //flag there.
         if(ODManager::IsInstanceCreated())
         {
            if(!odUsed)
            {
               computeTask = std::make_unique<ODComputeSummaryTask>();
               odUsed=true;
            }
            // PRL:  Is it correct to add all tracks to one task, even if they
            // are not partnered channels?  Rather than
            // make one task for each?
            computeTask->AddWaveTrack(wt->SharedPointer< WaveTrack >());
         }
      });
   }

   //add the task.
   if(odUsed)
      ODManager::Instance()->AddNewTask(std::move(computeTask));

   projectFileIO.AutoSave();
}

void ProjectHistory::SetStateTo(unsigned int n)
{
   auto &project = mProject;
   auto &undoManager = UndoManager::Get( project );

   undoManager.SetStateTo(n,
      [this]( const UndoState &state ){ PopState(state); } );
}
