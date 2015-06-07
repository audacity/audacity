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

#include <wx/hashset.h>

#include "BlockFile.h"
#include "Diags.h"
#include "Internat.h"
#include "Sequence.h"
#include "Track.h"
#include "WaveTrack.h"          // temp
#include "NoteTrack.h"  // for Sonify* function declarations
#include "Diags.h"

#include "UndoManager.h"

WX_DECLARE_HASH_SET(BlockFile *, wxPointerHash, wxPointerEqual, Set );

UndoManager::UndoManager()
{
   current = -1;
   saved = -1;
   consolidationCount = 0;
   ResetODChangesFlag();
}

UndoManager::~UndoManager()
{
   ClearStates();
}

void UndoManager::CalculateSpaceUsage()
{
   TIMER_START( "CalculateSpaceUsage", space_calc );
   TrackListOfKindIterator iter(Track::Wave);

   space.Clear();
   space.Add(0, stack.GetCount());

   Set *prev = new Set;
   Set *cur = new Set;

   for (size_t i = 0, cnt = stack.GetCount(); i < cnt; i++)
   {
      // Swap map pointers
      Set *swap = prev;
      prev = cur;
      cur = swap;

      // And clean out the new current map
      cur->clear();

      // Scan all tracks at current level
      WaveTrack *wt = (WaveTrack *) iter.First(stack[i]->tracks);
      while (wt)
      {
         // Scan all clips within current track
         WaveClipList::compatibility_iterator it = wt->GetClipIterator();
         while (it)
         {
            // Scan all blockfiles within current clip
            BlockArray *blocks = it->GetData()->GetSequenceBlockArray();
            for (size_t b = 0, cnt = blocks->GetCount(); b < cnt; b++)
            {
               BlockFile *file = blocks->Item(b)->f;

               // Accumulate space used by the file if the file didn't exist
               // in the previous level
               if (prev->count(file) == 0 && cur->count(file) == 0)
               {
                  space[i] += file->GetSpaceUsage().GetValue();
               }
               
               // Add file to current set
               cur->insert(file);
            }
            
            it = it->GetNext();
         }

         wt = (WaveTrack *) iter.Next();
      }
   }

   delete cur;
   delete prev;
   TIMER_STOP( space_calc );
}

wxLongLong_t UndoManager::GetLongDescription(unsigned int n, wxString *desc,
                                             wxString *size)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());
   wxASSERT(space.Count() == stack.Count());

   *desc = stack[n]->description;

   *size = Internat::FormatSize(space[n]);

   return space[n];
}

void UndoManager::GetShortDescription(unsigned int n, wxString *desc)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());

   *desc = stack[n]->shortDescription;
}

void UndoManager::SetLongDescription(unsigned int n, wxString desc)
{
   n -= 1;

   wxASSERT(n < stack.Count());

   stack[n]->description = desc;
}

void UndoManager::RemoveStateAt(int n)
{
   stack[n]->tracks->Clear(true);
   delete stack[n]->tracks;

   UndoStackElem *tmpStackElem = stack[n];
   stack.RemoveAt(n);
   delete tmpStackElem;
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
   RemoveStates(stack.Count());
}

unsigned int UndoManager::GetNumStates()
{
   return stack.Count();
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
   return (current < (int)stack.Count() - 1);
}

void UndoManager::ModifyState(TrackList * l,
                              const SelectedRegion &selectedRegion)
{
   if (current == wxNOT_FOUND) {
      return;
   }

   SonifyBeginModifyState();
   // Delete current
   stack[current]->tracks->Clear(true);
   delete stack[current]->tracks;

   // Duplicate
   TrackList *tracksCopy = new TrackList();
   TrackListIterator iter(l);
   Track *t = iter.First();
   while (t) {
      tracksCopy->Add(t->Duplicate());
      t = iter.Next();
   }

   // Replace
   stack[current]->tracks = tracksCopy;
   stack[current]->selectedRegion = selectedRegion;
   SonifyEndModifyState();
}

void UndoManager::PushState(TrackList * l,
                            const SelectedRegion &selectedRegion,
                            wxString longDescription,
                            wxString shortDescription,
                            int flags)
{
   unsigned int i;

   // If consolidate is set to true, group up to 3 identical operations.
   if (((flags&PUSH_CONSOLIDATE)!=0) && lastAction == longDescription &&
       consolidationCount < 2) {
      consolidationCount++;
      ModifyState(l, selectedRegion);
      // MB: If the "saved" state was modified by ModifyState, reset
      //  it so that UnsavedChanges returns true.
      if (current == saved) {
         saved = -1;
      }
      return;
   }

   consolidationCount = 0;

   i = current + 1;
   while (i < stack.Count()) {
      RemoveStateAt(i);
   }

   TrackList *tracksCopy = new TrackList();
   TrackListIterator iter(l);
   Track *t = iter.First();
   while (t) {
      tracksCopy->Add(t->Duplicate());
      t = iter.Next();
   }

   UndoStackElem *push = new UndoStackElem();
   push->tracks = tracksCopy;
   push->selectedRegion = selectedRegion;
   push->description = longDescription;
   push->shortDescription = shortDescription;

   stack.Add(push);
   current++;

   if (saved >= current) {
      saved = -1;
   }

   lastAction = longDescription;
}

TrackList *UndoManager::SetStateTo(unsigned int n,
                                   SelectedRegion *selectedRegion)
{
   n -= 1;

   wxASSERT(n < stack.Count());

   current = n;

   if (current == int(stack.Count()-1)) {
      *selectedRegion = stack[current]->selectedRegion;
   }
   else {
      *selectedRegion = stack[current + 1]->selectedRegion;
   }

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Undo(SelectedRegion *selectedRegion)
{
   wxASSERT(UndoAvailable());

   current--;

   *selectedRegion = stack[current]->selectedRegion;

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Redo(SelectedRegion *selectedRegion)
{
   wxASSERT(RedoAvailable());

   current++;

   *selectedRegion = stack[current]->selectedRegion;

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
   consolidationCount = 0;

   return stack[current]->tracks;
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
//   for (unsigned int i = 0; i < stack.Count(); i++) {
//      TrackListIterator iter(stack[i]->tracks);
//      WaveTrack *t = (WaveTrack *) (iter.First());
//      wxPrintf(wxT("*%d* %s %f\n"), i, (i == (unsigned int)current) ? wxT("-->") : wxT("   "),
//             t ? t->GetEndTime()-t->GetStartTime() : 0);
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
