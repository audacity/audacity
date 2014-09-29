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

#include "BlockFile.h"
#include "Internat.h"
#include "Sequence.h"
#include "Track.h"
#include "WaveTrack.h"          // temp
#include "NoteTrack.h"  // for Sonify* function declarations

#include <map>
#include <set>

#include "UndoManager.h"

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

// get the sum of the sizes of all blocks this track list
// references.  However, if a block is referred to multiple
// times it is only counted once.  Return value is in bytes.
wxLongLong UndoManager::CalculateSpaceUsage(int index)
{
   TrackListOfKindIterator iter(Track::Wave);
   WaveTrack *wt;
   WaveClipList::compatibility_iterator it;
   BlockArray *blocks;
   unsigned int i;

   // get a map of all blocks referenced in this TrackList
   std::map<BlockFile*, wxLongLong> cur;

   wt = (WaveTrack *) iter.First(stack[index]->tracks);
   while (wt) {
      for (it = wt->GetClipIterator(); it; it = it->GetNext()) {
         blocks = it->GetData()->GetSequenceBlockArray();
         for (i = 0; i < blocks->GetCount(); i++)
         {
            BlockFile* pBlockFile = blocks->Item(i)->f;
            if (pBlockFile->GetFileName().FileExists())
               cur[pBlockFile] = pBlockFile->GetSpaceUsage();
         }
      }
      wt = (WaveTrack *) iter.Next();
   }

   if (index > 0) {
      // get a set of all blocks referenced in all prev TrackList
      std::set<BlockFile*> prev;
      while (--index) {
         wt = (WaveTrack *) iter.First(stack[index]->tracks);
         while (wt) {
            for (it = wt->GetClipIterator(); it; it = it->GetNext()) {
               blocks = it->GetData()->GetSequenceBlockArray();
               for (i = 0; i < blocks->GetCount(); i++) {
                  prev.insert(blocks->Item(i)->f);
               }
            }
            wt = (WaveTrack *) iter.Next();
         }
      }

      // remove all blocks in prevBlockFiles from curBlockFiles
      std::set<BlockFile*>::const_iterator prevIter;
      for (prevIter = prev.begin(); prevIter != prev.end(); prevIter++) {
         cur.erase(*prevIter);
      }
   }

   // sum the sizes of the blocks remaining in curBlockFiles;
   wxLongLong bytes = 0;
   std::map<BlockFile*, wxLongLong>::const_iterator curIter;
   for (curIter = cur.begin(); curIter != cur.end(); curIter++) {
      bytes += curIter->second;
   }

   return bytes;
}

void UndoManager::GetLongDescription(unsigned int n, wxString *desc,
                                     wxString *size)
{
   n -= 1; // 1 based to zero based

   wxASSERT(n < stack.Count());

   *desc = stack[n]->description;

   *size = Internat::FormatSize(stack[n]->spaceUsage);
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

void UndoManager::ModifyState(TrackList * l, double sel0, double sel1)
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
   stack[current]->sel0 = sel0;
   stack[current]->sel1 = sel1;
   SonifyEndModifyState();
}

void UndoManager::PushState(TrackList * l, double sel0, double sel1,
                            wxString longDescription,
                            wxString shortDescription,
                            int flags)
{
   unsigned int i;

   // If consolidate is set to true, group up to 3 identical operations.
   if (((flags&PUSH_CONSOLIDATE)!=0) && lastAction == longDescription &&
       consolidationCount < 2) {
      consolidationCount++;
      ModifyState(l, sel0, sel1);
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
   push->sel0 = sel0;
   push->sel1 = sel1;
   push->description = longDescription;
   push->shortDescription = shortDescription;
   push->spaceUsage = 0; // Calculate actual value after it's on the stack.

   stack.Add(push);
   current++;
   if( (flags&PUSH_CALC_SPACE)!=0)
      push->spaceUsage = this->CalculateSpaceUsage(current);

   if (saved >= current) {
      saved = -1;
   }

   lastAction = longDescription;
}

TrackList *UndoManager::SetStateTo(unsigned int n, double *sel0, double *sel1)
{
   n -= 1;

   wxASSERT(n < stack.Count());

   current = n;

   if (current == int(stack.Count()-1)) {
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
   }
   else {
      current++;
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
      current--;
   }

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Undo(double *sel0, double *sel1)
{
   wxASSERT(UndoAvailable());

   current--;

   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

   lastAction = wxT("");
   consolidationCount = 0;

   return stack[current]->tracks;
}

TrackList *UndoManager::Redo(double *sel0, double *sel1)
{
   wxASSERT(RedoAvailable());

   current++;

   *sel0 = stack[current]->sel0;
   *sel1 = stack[current]->sel1;

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
