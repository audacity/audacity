/**********************************************************************

Audacity: A Digital Audio Editor

ProjectHistory.h

Paul Licameli split from ProjectManager.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_HISTORY__
#define __AUDACITY_PROJECT_HISTORY__

#include "ClientData.h"
#include "GlobalVariable.h"

class AudacityProject;
struct UndoState;
enum class UndoPush : unsigned char;

class PROJECT_HISTORY_API ProjectHistory final : public ClientData::Base
{
public:
    //! Type of function that saves project state to the database,
    //! or throws an exception on failure
    /*!
     Invoked when undo states are added or modified, or when the
     current state changes because of undo or redo
     */
    struct PROJECT_HISTORY_API AutoSave : GlobalHook<AutoSave,
                                                     void(AudacityProject&)
                                                     > {};

    static ProjectHistory& Get(AudacityProject& project);
    static const ProjectHistory& Get(const AudacityProject& project);

    explicit ProjectHistory(AudacityProject& project)
        : mProject{project}
    {}
    ProjectHistory(const ProjectHistory&) = delete;
    ProjectHistory& operator=(const ProjectHistory&) = delete;
    ~ProjectHistory() override;

    void InitialState();
    void SetStateTo(unsigned int n, bool doAutosave = true);
    bool UndoAvailable() const;
    bool RedoAvailable() const;
    void PushState(
        const TranslatableString& desc, const TranslatableString& shortDesc); // use UndoPush::AUTOSAVE
    void PushState(
        const TranslatableString& desc, const TranslatableString& shortDesc, UndoPush flags);
    void RollbackState();
    void ModifyState(bool bWantsAutoSave);   // if true, writes auto-save file.
    // Should set only if you really want the state change restored after
    // a crash, as it can take many seconds for large (eg. 10 track-hours)
    // projects
    void PopState(const UndoState& state, bool doAutosave = true);

    bool GetDirty() const { return mDirty; }
    void SetDirty(bool value) { mDirty = value; }

private:
    AudacityProject& mProject;

    bool mDirty{ false };
};

#endif
