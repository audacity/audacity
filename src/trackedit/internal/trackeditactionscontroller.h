/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actionable.h"
#include "framework/global/iinteractive.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "deletebehavioronboardingscenario.h"
#include "iprojecthistory.h"
#include "iselectioncontroller.h"
#include "itrackeditconfiguration.h"
#include "itrackeditinteraction.h"
#include "projectscene/iprojectsceneconfiguration.h"

#include "../itrackeditactionscontroller.h"

namespace au::trackedit {
class TrackeditActionsController : public ITrackeditActionsController, public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::ITrackeditConfiguration> configuration;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;

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
    void doGlobalCancel();
    void doGlobalSplit();
    void doGlobalSplitIntoNewTrack();
    void doGlobalJoin();
    void doGlobalDisjoin();
    void doGlobalDuplicate();

    void doGlobalCutPerClipRipple();
    void doGlobalCutPerTrackRipple();
    void doGlobalCutAllTracksRipple();

    void pasteDefault();
    void pasteOverlap();
    void pasteInsert();
    void pasteInsertRipple();

    void doGlobalDeletePerClipRipple();
    void doGlobalDeletePerTrackRipple();
    void doGlobalDeleteAllTracksRipple();

    void clipCut(const muse::actions::ActionData& args);
    void multiClipCut(const muse::actions::ActionData& args);
    void rangeSelectionCut(const muse::actions::ActionData& args);

    void clipCopy(const muse::actions::ActionData& args);
    void multiClipCopy();
    void rangeSelectionCopy();

    void clipDelete(const muse::actions::ActionData& args);
    void multiClipDelete(const muse::actions::ActionData& args);
    void rangeSelectionDelete(const muse::actions::ActionData& args);

    void trackSplit(const muse::actions::ActionData& args);
    void tracksSplitAt(const muse::actions::ActionData& args);
    void splitClipsAtSilences(const muse::actions::ActionData& args);
    void splitRangeSelectionAtSilences(const muse::actions::ActionData& args);
    void splitRangeSelectionIntoNewTracks(const muse::actions::ActionData& args);
    void splitClipsIntoNewTracks(const muse::actions::ActionData& args);
    void mergeSelectedOnTrack(const muse::actions::ActionData& args);
    void duplicateSelected(const muse::actions::ActionData& args);
    void duplicateClips(const muse::actions::ActionData& args);
    void clipSplitCut(const muse::actions::ActionData& args);
    void clipSplitDelete(const muse::actions::ActionData& args);
    void splitCutSelected(const muse::actions::ActionData& args);
    void splitDeleteSelected(const muse::actions::ActionData& args);

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
    void openClipPitchAndSpeed();
    void renderClipPitchAndSpeed(const muse::actions::ActionData& args);

    void swapStereoChannels(const muse::actions::ActionData& args);
    void splitStereoToLR(const muse::actions::ActionData& args);
    void splitStereoToCenter(const muse::actions::ActionData& args);
    void setCustomTrackRate(const muse::actions::ActionData& args);
    void makeStereoTrack(const muse::actions::ActionData& args);
    void resampleTracks(const muse::actions::ActionData& args);

    void groupClips();
    void ungroupClips();

    void selectAll();
    void selectNone();
    void selectAllTracks();
    void selectLeftOfPlaybackPos();
    void selectRightOfPlaybackPos();
    void selectTrackStartToCursor();
    void selectCursorToTrackEnd();
    void selectTrackStartToEnd();
    void moveCursorToClosestZeroCrossing();

    void setClipColor(const muse::actions::ActionQuery& q);
    void setTrackColor(const muse::actions::ActionQuery& q);
    void setTrackFormat(const muse::actions::ActionQuery& q);
    void setTrackRate(const muse::actions::ActionQuery& q);

    void changeTrackViewToWaveform(const muse::actions::ActionQuery&);
    void changeTrackViewToSpectrogram(const muse::actions::ActionQuery&);
    void changeTrackViewToWaveformAndSpectrogram(const muse::actions::ActionQuery&);
    void changeTrackView(const muse::actions::ActionQuery&, TrackViewType);

    void openTrackSpectrogramSettings(const muse::actions::ActionQuery&);

    void addLabel();

    void labelDelete(const muse::actions::ActionData& args);
    void labelDeleteMulti(const muse::actions::ActionData& args);

    void labelCut(const muse::actions::ActionData& args);
    void labelCutMulti(const muse::actions::ActionData& args);

    void labelCopy(const muse::actions::ActionData& args);
    void labelCopyMulti();

    context::IPlaybackStatePtr playbackState() const;

    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    DeleteBehaviorOnboardingScenario m_deleteBehaviorOnboardingScenario;
};
}
