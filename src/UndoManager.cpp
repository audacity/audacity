/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.cpp

  Dominic Mazzoni

*******************************************************************//**

\class UndoManager
\brief Works with HistoryWindow to provide the Undo functionality.

*//****************************************************************//**

\class UndoStackElem
\brief Holds one item with description and time range for the
UndoManager

*//*******************************************************************/


#include "Audacity.h"
#include "UndoManager.h"

#include <wx/hashset.h>

#include "BlockFile.h"
#include "Diags.h"
#include "Internat.h"
#include "Project.h"
#include "Sequence.h"
#include "WaveClip.h"
#include "WaveTrack.h"          // temp
#include "NoteTrack.h"  // for Sonify* function declarations
#include "Diags.h"
#include "Tags.h"


#include <unordered_set>

wxDEFINE_EVENT(EVT_UNDO_PUSHED, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_MODIFIED, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_RESET, wxCommandEvent);

using ConstBlockFilePtr = const BlockFile*;
using Set = std::unordered_set<ConstBlockFilePtr>;

struct UndoStackElem {

   UndoStackElem(std::shared_ptr<TrackList> &&tracks_,
      const wxString &description_,
      const wxString &shortDescription_,
      const SelectedRegion &selectedRegion_,
      const std::shared_ptr<Tags> &tags_)
      : state(std::move(tracks_), tags_, selectedRegion_)
      , description(description_)
      , shortDescription(shortDescription_)
   {
   }

   UndoState state;
   wxString description;
   wxString shortDescription;
};

UndoManager::UndoManager()
{
   current = -1;
   saved = -1;
   ResetODChangesFlag();
}

UndoManager::~UndoManager()
{
   ClearStates();
}

namespace {
   SpaceArray::value_type
   CalculateUsage(TrackList *tracks, Set *seen)
   {
      SpaceArray::value_type result = 0;

      //TIMER_START( "CalculateSpaceUsage", space_calc );
      for (auto wt : tracks->Any< WaveTrack >())
      {
         // Scan all clips within current track
         for(const auto &clip : wt->GetAllClips())
         {
            // Scan all blockfiles within current clip
            BlockArray *blocks = clip->GetSequenceBlockArray();
            for (const auto &block : *blocks)
            {
               const auto &file = block.f;

               // Accumulate space used by the file if the file was not
               // yet seen
               if ( !seen || (seen->count( &*file ) == 0 ) )
               {
                  unsigned long long usage{ file->GetSpaceUsage() };
                  result += usage;
               }

               // Add file to current set
               if (seen)
                  seen->insert( &*file );
            }
         }
      }

      return result;
   }
}

void UndoManager::CalculateSpaceUsage()
{
   space.clear();
   space.resize(stack.size(), 0);

   Set seen;

   // After copies and pastes, a block file may be used in more than
   // one place in one undo history state, and it may be used in more than
   // one undo history state.  It might even be used in two states, but not
   // in another state that is between them -- as when you have state A,
   // then make a cut to get state B, but then paste it back into state C.

   // So be sure to count each block file once only, in the last undo item that
   // contains it.

   // Why the last and not the first? Because the user of the History dialog
   // may DELETE undo states, oldest first.  To reclaim disk space you must
   // DELETE all states containing the block file.  So the block file's
   // contribution to space usage should be counted only in that latest state.

   for (size_t nn = stack.size(); nn--;)
   {
      // Scan all tracks at current level
      auto tracks = stack[nn]->state.tracks.get();
      space[nn] = CalculateUsage(tracks, &seen);
   }

   mClipboardSpaceUsage = CalculateUsage
      (AudacityProject::GetClipboardTracks(), nullptr);

   //TIMER_STOP( space_calc );
}

wxLongLong_t UndoManager::GetLongDescription(unsigned int n, wxString *desc,
                                             wxString *size)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.size());
   wxASSERT(space.size() == stack.size());

   *desc = stack[n]->description;

   *size = Internat::FormatSize(space[n]);

   return space[n];
}

void UndoManager::GetShortDescription(unsigned int n, wxString *desc)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.size());

   *desc = stack[n]->shortDescription;
}

void UndoManager::SetLongDescription(unsigned int n, const wxString &desc)
{
   n -= 1;

   wxASSERT(n < stack.size());

   stack[n]->description = desc;
}

void UndoManager::RemoveStateAt(int n)
{
   stack.erase(stack.begin() + n);
}


void UndoManager::RemoveStates(int num)
{
   for (int i = 0; i < num; i++) {
      RemoveStateAt(0);

      current -= 1;
      saved -= 1;
   }
}

void UndoManager::ClearStates()
{
   RemoveStates(stack.size());
   current = -1;
   saved = -1;
}

unsigned int UndoManager::GetNumStates()
{
   return stack.size();
}

unsigned int UndoManager::GetCurrentState()
{
   return current + 1;  // the array is 0 based, the abstraction is 1 based
}

bool UndoManager::UndoAvailable()
{
   return (current > 0);
}

bool UndoManager::RedoAvailable()
{
   return (current < (int)stack.size() - 1);
}

void UndoManager::ModifyState(const TrackList * l,
                              const SelectedRegion &selectedRegion,
                              const std::shared_ptr<Tags> &tags)
{
   if (current == wxNOT_FOUND) {
      return;
   }

   SonifyBeginModifyState();
   // Delete current -- not necessary, but let's reclaim space early
   stack[current]->state.tracks.reset();

   // Duplicate
   auto tracksCopy = TrackList::Create();
   for (auto t : *l) {
      if ( t->GetId() == TrackId{} )
         // Don't copy a pending added track
         continue;
      tracksCopy->Add(t->Duplicate());
   }

   // Replace
   stack[current]->state.tracks = std::move(tracksCopy);
   stack[current]->state.tags = tags;

   stack[current]->state.selectedRegion = selectedRegion;
   SonifyEndModifyState();

   // wxWidgets will own the event object
   QueueEvent( safenew wxCommandEvent{ EVT_UNDO_MODIFIED } );
}

void UndoManager::PushState(const TrackList * l,
                            const SelectedRegion &selectedRegion,
                            const std::shared_ptr<Tags> &tags,
                            const wxString &longDescription,
                            const wxString &shortDescription,
                            UndoPush flags)
{
   unsigned int i;

   if ( ((flags & UndoPush::CONSOLIDATE) != UndoPush::MINIMAL) &&
       lastAction == longDescription &&
       mayConsolidate ) {
      ModifyState(l, selectedRegion, tags);
      // MB: If the "saved" state was modified by ModifyState, reset
      //  it so that UnsavedChanges returns true.
      if (current == saved) {
         saved = -1;
      }
      return;
   }

   auto tracksCopy = TrackList::Create();
   for (auto t : *l) {
      if ( t->GetId() == TrackId{} )
         // Don't copy a pending added track
         continue;
      tracksCopy->Add(t->Duplicate());
   }

   mayConsolidate = true;

   i = current + 1;
   while (i < stack.size()) {
      RemoveStateAt(i);
   }

   // Assume tags was duplicted before any changes.
   // Just save a NEW shared_ptr to it.
   stack.push_back(
      std::make_unique<UndoStackElem>
         (std::move(tracksCopy),
            longDescription, shortDescription, selectedRegion, tags)
   );

   current++;

   if (saved >= current) {
      saved = -1;
   }

   lastAction = longDescription;

   // wxWidgets will own the event object
   QueueEvent( safenew wxCommandEvent{ EVT_UNDO_PUSHED } );
}

void UndoManager::SetStateTo(unsigned int n, const Consumer &consumer)
{
   n -= 1;

   wxASSERT(n < stack.size());

   current = n;

   lastAction = wxT("");
   mayConsolidate = false;

   consumer( stack[current]->state );

   // wxWidgets will own the event object
   QueueEvent( safenew wxCommandEvent{ EVT_UNDO_RESET } );
}

void UndoManager::Undo(const Consumer &consumer)
{
   wxASSERT(UndoAvailable());

   current--;

   lastAction = wxT("");
   mayConsolidate = false;

   consumer( stack[current]->state );

   // wxWidgets will own the event object
   QueueEvent( safenew wxCommandEvent{ EVT_UNDO_RESET } );
}

void UndoManager::Redo(const Consumer &consumer)
{
   wxASSERT(RedoAvailable());

   current++;

   /*
   if (!RedoAvailable()) {
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
   }
   else {
      current++;
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
      current--;
   }
   */

   lastAction = wxT("");
   mayConsolidate = false;

   consumer( stack[current]->state );

   // wxWidgets will own the event object
   QueueEvent( safenew wxCommandEvent{ EVT_UNDO_RESET } );
}

bool UndoManager::UnsavedChanges()
{
   return (saved != current) || HasODChangesFlag();
}

void UndoManager::StateSaved()
{
   saved = current;
   ResetODChangesFlag();
}

// currently unused
//void UndoManager::Debug()
//{
//   for (unsigned int i = 0; i < stack.size(); i++) {
//      for (auto t : stack[i]->tracks->Any())
//         wxPrintf(wxT("*%d* %s %f\n"),
//                  i, (i == (unsigned int)current) ? wxT("-->") : wxT("   "),
//                t ? t->GetEndTime()-t->GetStartTime() : 0);
//   }
//}

///to mark as unsaved changes without changing the state/tracks.
void UndoManager::SetODChangesFlag()
{
   mODChangesMutex.Lock();
   mODChanges=true;
   mODChangesMutex.Unlock();
}

bool UndoManager::HasODChangesFlag()
{
   bool ret;
   mODChangesMutex.Lock();
   ret=mODChanges;
   mODChangesMutex.Unlock();
   return ret;
}

void UndoManager::ResetODChangesFlag()
{
   mODChangesMutex.Lock();
   mODChanges=false;
   mODChangesMutex.Unlock();
}
