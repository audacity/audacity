/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.h

  Dominic Mazzoni

  After each operation, call UndoManager's PushState.

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

  Undo() temporarily moves down one state.
  If another PushState is called, the redo information is lost.

  Redo()

  UndoAvailable()

  RedoAvailable()

**********************************************************************/

#ifndef __AUDACITY_UNDOMANAGER__
#define __AUDACITY_UNDOMANAGER__

#include <functional>
#include <memory>
#include <vector>
#include "ClientData.h"
#include "Observer.h"

//! Type of message published by UndoManager
/*! all are published only during idle time, except BeginPurge and EndPurge */
struct UndoRedoMessage {
    const enum Type {
        Pushed, /*!< Project state did not change, but a new state was copied
               into Undo history and any redo states were lost */
        Modified, /*!< Project state did not change, but current state was
                 modified in Undo history */
        Renamed, /*!< Project state did not change, but current state was renamed
                in Undo history */
        UndoOrRedo, /*!< Project state changed because of undo or redo; undo
                   manager contents did not change other than the pointer to
                   current state */
        Reset, /*!< Project state changed other than for single-step undo/redo;
              undo manager contents did not change other than the pointer to
              current state */
        Purge, //!< Undo or redo states eliminated

        // Eagerly sent messages (not waiting for idle time)
        BeginPurge, //!< Begin elimination of old undo states
        EndPurge, //!< End elimination of old undo states
    } type;

    //! Only significant for BeginPurge messages
    const size_t begin = 0, end = 0;
};

class AudacityProject;

//! Base class for extra information attached to undo/redo states
class PROJECT_HISTORY_API UndoStateExtension
{
public:
    virtual ~UndoStateExtension();

    //! Modify the project when undoing or redoing to some state in history
    virtual void RestoreUndoRedoState(AudacityProject& project) = 0;

    //! Whether undo or redo is now permitted; default returns true
    virtual bool CanUndoOrRedo(const AudacityProject& project);
};

class PROJECT_HISTORY_API UndoRedoExtensionRegistry
{
public:
    //! Type of function that produces an UndoStateExtension object when saving state of a project
    /*! Shared pointer allows easy sharing of unchanging parts of project state among history states */
    using Saver
        =std::function<std::shared_ptr<UndoStateExtension>(AudacityProject&)>;

    //! Typically statically constructed
    struct PROJECT_HISTORY_API Entry {
        Entry(const Saver& saver);
    };
};

struct UndoState {
    using Extensions = std::vector<std::shared_ptr<UndoStateExtension> >;

    UndoState(Extensions extensions)
        : extensions(std::move(extensions))
    {}

    Extensions extensions;
};

struct UndoStackElem {
    UndoStackElem(UndoState::Extensions extensions,
                  const TranslatableString& description_,
                  const TranslatableString& shortDescription_)
        : state(std::move(extensions))
        , description(description_)
        , shortDescription(shortDescription_)
    {
    }

    UndoState state;
    TranslatableString description;
    TranslatableString shortDescription;
};

using UndoStack = std::vector <std::unique_ptr<UndoStackElem> >;

// These flags control what extra to do on a PushState
// Default is AUTOSAVE
// Frequent/faster actions use CONSOLIDATE
enum class UndoPush : unsigned char {
    NONE = 0,
    CONSOLIDATE = 1 << 0,
    NOAUTOSAVE = 1 << 1
};

inline UndoPush operator |(UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) | static_cast<int>(b)); }
inline UndoPush operator &(UndoPush a, UndoPush b)
{ return static_cast<UndoPush>(static_cast<int>(a) & static_cast<int>(b)); }

//! Maintain a non-persistent list of states of the project, to support undo and redo commands
/*! The history should be cleared before destruction */
class PROJECT_HISTORY_API UndoManager final : public ClientData::Base, public Observer::Publisher<UndoRedoMessage>,
    public std::enable_shared_from_this<UndoManager>
{
public:
    static UndoManager& Get(AudacityProject& project);
    static const UndoManager& Get(const AudacityProject& project);

    explicit
    UndoManager(AudacityProject& project);
    ~UndoManager();

    UndoManager(const UndoManager&) = delete;
    UndoManager& operator =(const UndoManager&) = delete;

    void PushState(const TranslatableString& longDescription, const TranslatableString& shortDescription, UndoPush flags = UndoPush::NONE);
    void ModifyState();
    void RenameState(int state, const TranslatableString& longDescription, const TranslatableString& shortDescription);
    void AbandonRedo();
    void ClearStates();
    void RemoveStates(
        size_t begin, //!< inclusive start of range
        size_t end  //!< exclusive end of range
        );
    unsigned int GetNumStates();
    unsigned int GetCurrentState();

    void StopConsolidating() { mayConsolidate = false; }

    void GetShortDescription(unsigned int n, TranslatableString* desc);
    void SetLongDescription(unsigned int n, const TranslatableString& desc);

    // These functions accept a callback that uses the state,
    // and then they send to the project Reset or UndoOrRedo when
    // that has finished.
    using Consumer = std::function< void (const UndoStackElem&) >;
    void SetStateTo(unsigned int n, const Consumer& consumer);
    void Undo(const Consumer& consumer);
    void Redo(const Consumer& consumer);

    //! Give read-only access to all states
    void VisitStates(const Consumer& consumer, bool newestFirst);
    //! Visit a specified range of states
    /*! end is exclusive; visit newer states first if end < begin */
    void VisitStates(
        const Consumer& consumer, size_t begin, size_t end);

    bool UndoAvailable();
    bool RedoAvailable();

    void MarkUnsaved();
    bool UnsavedChanges() const;
    int GetSavedState() const;
    void StateSaved();

    // void Debug(); // currently unused

private:
    bool CheckAvailable(int index);

    void EnqueueMessage(UndoRedoMessage message);
    void RemoveStateAt(int n);

    AudacityProject& mProject;

    int current;
    int saved;

    UndoStack stack;

    TranslatableString lastAction;
    bool mayConsolidate { false };
};

#endif
