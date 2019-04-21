/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.h

  Dominic Mazzoni

  After each operation, call UndoManager's PushState, pass it
  the entire track hierarchy.  The UndoManager makes a duplicate
  of every single track using its Duplicate method, which should
  increment reference counts.  If we were not at the top of
  the stack when this is called, DELETE above first.

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
  PushState is true, then NEW changes may accumulate into the most
  recent Undo state, if descriptions match and if no Undo or Redo or rollback
  operation intervened since that state was pushed.

  Undo() temporarily moves down one state and returns the track
  hierarchy.  If another PushState is called, the redo information
  is lost.

  Redo()

  UndoAvailable()

  RedoAvailable()

**********************************************************************/

#ifndef __AUDACITY_UNDOMANAGER__
#define __AUDACITY_UNDOMANAGER__

#include "MemoryX.h"
#include <vector>
#include <wx/event.h> // to declare custom event types
#include "ondemand/ODTaskThread.h"
#include "SelectedRegion.h"

// Events emitted by UndoManager for the use of listeners

// Project state did not change, but a new state was copied into Undo history
// and any redo states were lost
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_PUSHED, wxCommandEvent);

// Project state did not change, but current state was modified in Undo history
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_MODIFIED, wxCommandEvent);

// Project state changed because of undo or redo or rollback; undo manager
// contents did not change other than the pointer to current state
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_RESET, wxCommandEvent);

class Tags;
class Track;
class TrackList;

struct UndoStackElem;
struct UndoState {
   UndoState(std::shared_ptr<TrackList> &&tracks_,
      const std::shared_ptr<Tags> &tags_,
      const SelectedRegion &selectedRegion_)
      : tracks(std::move(tracks_)), tags(tags_), selectedRegion(selectedRegion_)
   {}

   std::shared_ptr<TrackList> tracks;
   std::shared_ptr<Tags> tags;
   SelectedRegion selectedRegion; // by value
};

using UndoStack = std::vector <std::unique_ptr<UndoStackElem>>;

using SpaceArray = std::vector <unsigned long long> ;

// These flags control what extra to do on a PushState
// Default is AUTOSAVE
// Frequent/faster actions use CONSOLIDATE
enum class UndoPush : unsigned char {
   MINIMAL = 0,
   CONSOLIDATE = 1 << 0,
   AUTOSAVE = 1 << 1
};

inline UndoPush operator | (UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) | static_cast<int>(b)); }
inline UndoPush operator & (UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) & static_cast<int>(b)); }

class AUDACITY_DLL_API UndoManager : public wxEvtHandler {
 public:
   UndoManager();
   ~UndoManager();

   UndoManager( const UndoManager& ) = delete;
   UndoManager& operator = ( const UndoManager& ) = delete;

   void PushState(const TrackList * l,
                  const SelectedRegion &selectedRegion,
                  const std::shared_ptr<Tags> &tags,
                  const wxString &longDescription, const wxString &shortDescription,
                  UndoPush flags = UndoPush::AUTOSAVE);
   void ModifyState(const TrackList * l,
                    const SelectedRegion &selectedRegion, const std::shared_ptr<Tags> &tags);
   void ClearStates();
   void RemoveStates(int num);  // removes the 'num' oldest states
   void RemoveStateAt(int n);   // removes the n'th state (1 is oldest)
   unsigned int GetNumStates();
   unsigned int GetCurrentState();

   void StopConsolidating() { mayConsolidate = false; }

   void GetShortDescription(unsigned int n, wxString *desc);
   // Return value must first be calculated by CalculateSpaceUsage():
   wxLongLong_t GetLongDescription(unsigned int n, wxString *desc, wxString *size);
   void SetLongDescription(unsigned int n, const wxString &desc);

   // These functions accept a callback that uses the state,
   // and then they emit EVT_UNDO_RESET when that has finished.
   using Consumer = std::function< void( const UndoState & ) >;
   void SetStateTo(unsigned int n, const Consumer &consumer);
   void Undo(const Consumer &consumer);
   void Redo(const Consumer &consumer);

   bool UndoAvailable();
   bool RedoAvailable();

   bool UnsavedChanges();
   void StateSaved();

   // Return value must first be calculated by CalculateSpaceUsage():
   // The clipboard is global, not specific to this project, but it is
   // convenient to combine the space usage calculations in one class:
   wxLongLong_t GetClipboardSpaceUsage() const
   { return mClipboardSpaceUsage; }

   void CalculateSpaceUsage();

   // void Debug(); // currently unused

   ///to mark as unsaved changes without changing the state/tracks.
   void SetODChangesFlag();
   bool HasODChangesFlag();
   void ResetODChangesFlag();

 private:
   int current;
   int saved;
   UndoStack stack;

   wxString lastAction;
   bool mayConsolidate { false };

   SpaceArray space;
   unsigned long long mClipboardSpaceUsage {};

   bool mODChanges;
   ODLock mODChangesMutex;//mODChanges is accessed from many threads.

};

#endif
