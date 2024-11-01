/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iprojecthistory.h"
#include "itrackeditinteraction.h"
#include "iselectioncontroller.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "../itrackeditactionscontroller.h"

namespace au::trackedit {
class TrackeditActionsController : public ITrackeditActionsController, public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    void init();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void undo();
    void redo();

    void doGlobalCopy();
    void doGlobalCut();
    void doGlobalDelete();
    void doGlobalSplitCut();
    void doGlobalSplitDelete();
    void doGlobalSplit();
    void doGlobalJoin();
    void doGlobalDuplicate();

    void clipCut(const muse::actions::ActionData& args);
    void clipCopy(const muse::actions::ActionData& args);
    void clipDelete(const muse::actions::ActionData& args);

    void clipCutSelected();
    void clipCopySelected();
    void clipDeleteSelected();

    void paste();

    void trackSplit(const muse::actions::ActionData& args);
    void tracksSplitAt(const muse::actions::ActionData& args);
    void mergeSelectedOnTrack(const muse::actions::ActionData& args);
    void duplicateSelected(const muse::actions::ActionData& args);
    void duplicateClip(const muse::actions::ActionData& args);
    void clipSplitCut(const muse::actions::ActionData& args);
    void clipSplitDelete(const muse::actions::ActionData& args);
    void splitCutSelected(const muse::actions::ActionData& args);
    void splitDeleteSelected(const muse::actions::ActionData& args);

    void toggleLoopRegion();
    void clearLoopRegion();
    void setLoopRegionToSelection();
    void setSelectionToLoop();
    void setLoopRegionIn();
    void setLoopRegionOut();

    void newMonoTrack();
    void newStereoTrack();
    void newLabelTrack();

    void deleteTracks(const muse::actions::ActionData&);
    void duplicateTracks(const muse::actions::ActionData&);

    void moveTracksUp(const muse::actions::ActionData& args);
    void moveTracksDown(const muse::actions::ActionData& args);
    void moveTracksToTop(const muse::actions::ActionData& args);
    void moveTracksToBottom(const muse::actions::ActionData& args);

    void trimAudioOutsideSelection();
    void silenceAudioSelection();

    void pushProjectHistoryTrackAddedState();
    void pushProjectHistoryTrackTrimState(secs_t start, secs_t end);
    void pushProjectHistoryTrackSilenceState(secs_t start, secs_t end);
    void pushProjectHistoryPasteState();
    void pushProjectHistoryDeleteState(secs_t start, secs_t duration);

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}
