/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.h

  Dominic Mazzoni

  After each operation, call UndoManager's PushState, pass it
  the entire track hierarchy.  The UndoManager makes a duplicate
  of every single track using its Duplicate method, which should
  increment reference counts.  If we were not at the top of
  the stack when this is called, delete above first.

  If a minor change is made, for example changing the visual
  display of a track or changing the selection, you can call
  ModifyState, which replaces the current state with the
  one you give it, without deleting everything above it.

  Each action has a long description and a short description
  associated with it.  The long description appears in the
  History window and should be a complete sentence in the
  past tense, for example, "Deleted 2 seconds.".  The short
  description should be one or two words at most, all
  capitalized, and should represent the name of the command.
  It will be appended on the end of the word "Undo" or "Redo",
  for example the short description of "Deleted 2 seconds."
  would just be "Delete", resulting in menu titles
  "Undo Delete" and "Redo Delete".

  UndoManager can also automatically consolidate actions into
  a single state change.  If the "consolidate" argument to
  PushState is true, then up to 3 identical events in a row
  will result in one PushState and 2 ModifyStates.

  Undo() temporarily moves down one state and returns the track
  hierarchy.  If another PushState is called, the redo information
  is lost.

  Redo()

  UndoAvailable()

  RedoAvailable()

**********************************************************************/

#ifndef __AUDACITY_UNDOMANAGER__
#define __AUDACITY_UNDOMANAGER__

#include <wx/dynarray.h>
#include <wx/string.h>
#include "ondemand/ODTaskThread.h"

class Track;
class TrackList;

struct UndoStackElem {
   TrackList *tracks;
   wxString description;
   wxString shortDescription;
   double sel0;
   double sel1;
   wxLongLong spaceUsage;
};

WX_DEFINE_USER_EXPORTED_ARRAY(UndoStackElem *, UndoStack, class AUDACITY_DLL_API);

// These flags control what extra to do on a PushState
// Default is PUSH_AUTOSAVE | PUSH_CALC_SPACE
// Frequent/faster actions use PUSH_CONSOLIDATE
const int PUSH_MINIMAL = 0;
const int PUSH_CONSOLIDATE = 1;
const int PUSH_CALC_SPACE = 2;
const int PUSH_AUTOSAVE = 4;

class AUDACITY_DLL_API UndoManager {
 public:
   UndoManager();
   ~UndoManager();

   void PushState(TrackList * l, double sel0, double sel1,
                  wxString longDescription, wxString shortDescription,
                  int flags = PUSH_CALC_SPACE|PUSH_AUTOSAVE );
   void ModifyState(TrackList * l, double sel0, double sel1);
   void ClearStates();
   void RemoveStates(int num);  // removes the 'num' oldest states
   void RemoveStateAt(int n);   // removes the n'th state (1 is oldest)
   unsigned int GetNumStates();
   unsigned int GetCurrentState();

   void GetShortDescription(unsigned int n, wxString *desc);
   void GetLongDescription(unsigned int n, wxString *desc, wxString *size);
   void SetLongDescription(unsigned int n, wxString desc);

   TrackList *SetStateTo(unsigned int n, double *sel0, double *sel1);
   TrackList *Undo(double *sel0, double *sel1);
   TrackList *Redo(double *sel0, double *sel1);

   bool UndoAvailable();
   bool RedoAvailable();

   bool UnsavedChanges();
   void StateSaved();

   // void Debug(); // currently unused

   ///to mark as unsaved changes without changing the state/tracks.
   void SetODChangesFlag();
   bool HasODChangesFlag();
   void ResetODChangesFlag();

 private:
   wxLongLong CalculateSpaceUsage(int index);

   int current;
   int saved;
   UndoStack stack;

   wxString lastAction;
   int consolidationCount;

   bool mODChanges;
   ODLock mODChangesMutex;//mODChanges is accessed from many threads.

};

#endif
