/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "projectscene/iprojectsceneconfiguration.h"
#include "iprojecthistory.h"
#include "itrackeditinteraction.h"
#include "iselectioncontroller.h"

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
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;

public:
    void init();

    bool actionEnabled(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& actionCode) const override;

private:
    void notifyActionEnabledChanged(const muse::actions::ActionCode& actionCode);
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
    void multiClipCut();
    void rangeSelectionCut();

    void clipCopy(const muse::actions::ActionData& args);
    void multiClipCopy();
    void rangeSelectionCopy();

    void clipDelete(const muse::actions::ActionData& args);
    void multiClipDelete();
    void rangeSelectionDelete();

    void paste();

    void trackSplit(const muse::actions::ActionData& args);
    void tracksSplitAt(const muse::actions::ActionData& args);
    void mergeSelectedOnTrack(const muse::actions::ActionData& args);
    void duplicateSelected(const muse::actions::ActionData& args);
    void duplicateClips(const muse::actions::ActionData& args);
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

    void toggleStretchClipToMatchTempo(const muse::actions::ActionData& args);
    void renderClipPitchAndSpeed(const muse::actions::ActionData& args);

    void groupClips();
    void ungroupClips();

    void setClipColor(const muse::actions::ActionQuery& q);

    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}
