/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/ui/inavigationcontroller.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "spectrogram/ifrequencyselectioncontroller.h"
#include "spectrogram/ispectraleffectsregister.h"
#include "iprojecthistory.h"
#include "iselectioncontroller.h"
#include "itrackeditconfiguration.h"
#include "itrackeditinteraction.h"
#include "internal/itracknavigationcontroller.h"

#include "deletebehavioronboardingscenario.h"

#include "../itrackeditactionscontroller.h"

namespace au::trackedit {
class TrackeditActionsController : public ITrackeditActionsController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::GlobalInject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::GlobalInject<trackedit::ITrackeditConfiguration> configuration;
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<audio::IAudioDevicesProvider> audioDevicesProvider { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction { this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };
    muse::ContextInject<muse::ui::INavigationController> navigationController { this };
    muse::ContextInject<spectrogram::IFrequencySelectionController> frequencySelectionController { this };

public:
    TrackeditActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx), m_deleteBehaviorOnboardingScenario(ctx) {}

    void init();

    bool actionEnabled(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& actionCode) const override;

private:
    void notifyActionEnabledChanged(const muse::actions::ActionCode& actionCode);
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    bool isFocusedItemClip() const;
    ClipKeyList clipsForInteraction() const;

    bool isFocusedItemLabel() const;
    LabelKeyList labelsForInteraction() const;

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

    void doGlobalCutLeaveGap();
    void doGlobalCutPerClipRipple();
    void doGlobalCutPerTrackRipple();
    void doGlobalCutAllTracksRipple();

    void pasteDefault();
    void pasteOverlap();
    void pasteInsert();
    void pasteInsertRipple();

    void doGlobalDeleteLeaveGap();
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
    void resetClipPitchAndSpeed(const muse::actions::ActionData& args);

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
    void setSelection(const muse::actions::ActionQuery& query);
    void selectTrackByIndex(const muse::actions::ActionQuery& query);
    void moveCursorToClosestZeroCrossing();

    void setClipColor(const muse::actions::ActionQuery& q);
    void setTrackColor(const muse::actions::ActionQuery& q);
    void setTrackFormat(const muse::actions::ActionQuery& q);
    void setTrackRate(const muse::actions::ActionQuery& q);

    void toggleGlobalSpectrogramView();
    void changeTrackViewToWaveform(const muse::actions::ActionQuery&);
    void changeTrackViewToSpectrogram(const muse::actions::ActionQuery&);
    void changeTrackViewToWaveformAndSpectrogram(const muse::actions::ActionQuery&);
    void changeTrackView(const muse::actions::ActionQuery&, TrackViewType);

    void addLabel();

    void labelDeleteMulti(const muse::actions::ActionData& args);
    void labelCutMulti(const muse::actions::ActionData& args);
    void labelCopyMulti();

    void moveFocusedItemLeft();
    void moveFocusedItemRight();
    void moveFocusedItemUp();
    void moveFocusedItemDown();
    void extendFocusedItemBoundaryLeft();
    void extendFocusedItemBoundaryRight();
    void reduceFocusedItemBoundaryLeft();
    void reduceFocusedItemBoundaryRight();

    double zoomLevel() const;
    double calculateStepSize() const;
    Label focusedLabel() const;
    TrackId resolvePreviousTrackIdForMove(const TrackId& trackId) const;
    TrackId resolveNextTrackIdForMove(const TrackId& trackId) const;

    context::IPlaybackStatePtr playbackState() const;

    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    DeleteBehaviorOnboardingScenario m_deleteBehaviorOnboardingScenario;
};
}
