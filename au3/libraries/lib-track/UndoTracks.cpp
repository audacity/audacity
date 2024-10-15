/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file UndoTracks.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "UndoTracks.h"
#include "PendingTracks.h"
#include "Track.h"
#include "UndoManager.h"

// Undo/redo handling of selection changes
namespace {
struct TrackListRestorer final : UndoStateExtension {
   TrackListRestorer(AudacityProject &project)
      : mpTracks{ TrackList::Create(nullptr) }
   {
      for (auto pTrack : TrackList::Get(project)) {
         if (pTrack->GetId() == TrackId{})
            // Don't copy a pending added track
            continue;
         mpTracks->Add(pTrack->Duplicate());
      }
   }
   void RestoreUndoRedoState(AudacityProject &project) override {
      auto &dstTracks = TrackList::Get(project);
      dstTracks.Clear();
      for (auto pTrack : *mpTracks)
         dstTracks.Add(pTrack->Duplicate());
   }
   bool CanUndoOrRedo(const AudacityProject &project) override {
      return !PendingTracks::Get(project).HasPendingTracks();
   }
   const std::shared_ptr<TrackList> mpTracks;
};

UndoRedoExtensionRegistry::Entry sEntry {
   [](AudacityProject &project) -> std::shared_ptr<UndoStateExtension> {
      return std::make_shared<TrackListRestorer>(project);
   }
};
}

TrackList *UndoTracks::Find(const UndoStackElem &state)
{
   auto &exts = state.state.extensions;
   auto end = exts.end(),
      iter = std::find_if(exts.begin(), end, [](auto &pExt){
         return dynamic_cast<TrackListRestorer*>(pExt.get());
      });
   if (iter != end)
      return static_cast<TrackListRestorer*>(iter->get())->mpTracks.get();
   return nullptr;
}
