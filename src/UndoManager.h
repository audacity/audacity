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

#include <vector>
#include <wx/event.h> // to declare custom event types
#include "ClientData.h"
#include "SelectedRegion.h"

// Events emitted by AudacityProject for the use of listeners

// Project state did not change, but a new state was copied into Undo history
// and any redo states were lost
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_PUSHED, wxCommandEvent);

// Project state did not change, but current state was modified in Undo history
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_MODIFIED, wxCommandEvent);

// Project state did not change, but current state was renamed in Undo history
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_RENAMED, wxCommandEvent);

// Project state changed because of undo or redo; undo manager
// contents did not change other than the pointer to current state
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_OR_REDO, wxCommandEvent);

// Project state changed other than for single-step undo/redo; undo manager
// contents did not change other than the pointer to current state
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_RESET, wxCommandEvent);

// Undo or redo states discarded
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_UNDO_PURGE, wxCommandEvent);

class AudacityProject;
class Tags;
class Track;
class TrackList;

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

struct UndoStackElem {

   UndoStackElem(std::shared_ptr<TrackList> &&tracks_,
      const TranslatableString &description_,
      const TranslatableString &shortDescription_,
      const SelectedRegion &selectedRegion_,
      const std::shared_ptr<Tags> &tags_)
      : state(std::move(tracks_), tags_, selectedRegion_)
      , description(description_)
      , shortDescription(shortDescription_)
   {
   }

   UndoState state;
   TranslatableString description;
   TranslatableString shortDescription;
};

using UndoStack = std::vector <std::unique_ptr<UndoStackElem>>;

using SpaceArray = std::vector <unsigned long long> ;

// These flags control what extra to do on a PushState
// Default is AUTOSAVE
// Frequent/faster actions use CONSOLIDATE
enum class UndoPush : unsigned char {
   NONE = 0,
   CONSOLIDATE = 1 << 0,
   NOAUTOSAVE = 1 << 1
};

inline UndoPush operator | (UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) | static_cast<int>(b)); }
inline UndoPush operator & (UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) & static_cast<int>(b)); }

//! Maintain a non-persistent list of states of the project, to support undo and redo commands
/*! The history should be cleared before destruction */
class AUDACITY_DLL_API UndoManager final
   : public ClientData::Base
{
 public:
   static UndoManager &Get( AudacityProject &project );
   static const UndoManager &Get( const AudacityProject &project );
 
   explicit
   UndoManager( AudacityProject &project );
   ~UndoManager();

   UndoManager( const UndoManager& ) = delete;
   UndoManager& operator = ( const UndoManager& ) = delete;

   void PushState(const TrackList * l,
                  const SelectedRegion &selectedRegion,
                  const std::shared_ptr<Tags> &tags,
                  const TranslatableString &longDescription,
                  const TranslatableString &shortDescription,
                  UndoPush flags = UndoPush::NONE);
   void ModifyState(const TrackList * l,
                    const SelectedRegion &selectedRegion, const std::shared_ptr<Tags> &tags);
   void RenameState( int state,
      const TranslatableString &longDescription,
      const TranslatableString &shortDescription);
   void AbandonRedo();
   void ClearStates();
   void RemoveStates(
      size_t begin, //!< inclusive start of range
      size_t end    //!< exclusive end of range
   );
   unsigned int GetNumStates();
   unsigned int GetCurrentState();

   void StopConsolidating() { mayConsolidate = false; }

   void GetShortDescription(unsigned int n, TranslatableString *desc);
   // Return value must first be calculated by CalculateSpaceUsage():
   wxLongLong_t GetLongDescription(
      unsigned int n, TranslatableString *desc, TranslatableString *size);
   void SetLongDescription(unsigned int n, const TranslatableString &desc);

   // These functions accept a callback that uses the state,
   // and then they send to the project EVT_UNDO_RESET or EVT_UNDO_OR_REDO when
   // that has finished.
   using Consumer = std::function< void( const UndoStackElem & ) >;
   void SetStateTo(unsigned int n, const Consumer &consumer);
   void Undo(const Consumer &consumer);
   void Redo(const Consumer &consumer);

   //! Give read-only access to all states
   void VisitStates( const Consumer &consumer, bool newestFirst );
   //! Visit a specified range of states
   /*! end is exclusive; visit newer states first if end < begin */
   void VisitStates(
      const Consumer &consumer, size_t begin, size_t end );

   bool UndoAvailable();
   bool RedoAvailable();

   bool UnsavedChanges() const;
   int GetSavedState() const;
   void StateSaved();

   // Return value must first be calculated by CalculateSpaceUsage():
   // The clipboard is global, not specific to this project, but it is
   // convenient to combine the space usage calculations in one class:
   wxLongLong_t GetClipboardSpaceUsage() const
   { return mClipboardSpaceUsage; }

   void CalculateSpaceUsage();

   // void Debug(); // currently unused

 private:
   size_t EstimateRemovedBlocks(size_t begin, size_t end);

   void RemoveStateAt(int n);

   AudacityProject &mProject;
 
   int current;
   int saved;
   UndoStack stack;

   TranslatableString lastAction;
   bool mayConsolidate { false };

   SpaceArray space;
   unsigned long long mClipboardSpaceUsage {};
};

#endif
