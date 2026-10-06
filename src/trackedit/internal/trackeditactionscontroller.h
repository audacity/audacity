/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <optional>

#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"
#include "framework/rcommand/commandable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/ui/inavigationcontroller.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "context/iglobalcontext.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "spectrogram/ifrequencyselectioncontroller.h"
#include "spectrogram/ispectraleffectsregister.h"
#include "../itracksviewrequestsservice.h"
#include "../iprojecthistory.h"
#include "../iselectioncontroller.h"
#include "../itrackeditconfiguration.h"
#include "../itrackeditinteraction.h"
#include "itracknavigationcontroller.h"

#include "deletebehavioronboardingscenario.h"

#include "../itrackeditactionscontroller.h"

namespace au::trackedit {
class TrackeditActionsController : public ITrackeditActionsController, public muse::actions::Actionable, public muse::rcommand::Commandable,
    public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::GlobalInject<trackedit::ITrackeditConfiguration> configuration;
    muse::GlobalInject<spectrogram::ISpectralEffectsRegister> spectralEffectsRegister;
    muse::GlobalInject<audio::IAudioDriverController> audioDriverController;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction { this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };
    muse::ContextInject<trackedit::ITracksViewRequestsService> tracksViewRequestsService { this };
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
    friend class TrackeditActionsControllerTests;

    void notifyActionEnabledChanged(const muse::actions::ActionCode& actionCode);
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    bool isFocusedItemClip() const;
    ClipKeyList clipsForInteraction() const;
    bool canSilenceAudio() const;

    struct ContiguousClipsSpan {
        TrackId trackId = INVALID_TRACK;
        secs_t begin = 0.0;
        secs_t end = 0.0;
    };
    std::optional<ContiguousClipsSpan> contiguousSelectedClipsSpan() const;
    bool rangeSelectionCoversMultipleClips() const;

    bool isFocusedItemLabel() const;
    LabelKeyList labelsForInteraction() const;

    TrackId currentFocusedOrSelectedTrack() const;
    void focusTrack(const TrackId& trackId);
    bool stepFocusOutOfSelection(const TrackItemKeyList& selectedItems, const TrackItemKey& focusedItem, const TrackId& currentTrack,
                                 const std::function<void()>& resetSelection);

    void doGlobalCopy();
    void doGlobalCut();
    void doGlobalDelete();
    muse::Ret doGlobalCancel();
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
    muse::Ret pasteOverlap();
    muse::Ret pasteInsert();
    muse::Ret pasteInsertRipple();

    void doGlobalDeleteLeaveGap();
    void doGlobalDeletePerClipRipple();
    void doGlobalDeletePerTrackRipple();
    void doGlobalDeleteAllTracksRipple();

    void multiClipCut(const muse::actions::ActionData& args);
    void rangeSelectionCut(const muse::actions::ActionData& args);

    void multiClipCopy();
    void rangeSelectionCopy();

    void multiClipDelete(const muse::actions::ActionData& args);
    void rangeSelectionDelete(const muse::actions::ActionData& args);

    void trackSplit(const muse::actions::ActionData& args);
    muse::Ret tracksSplitAt(const muse::rcommand::Params& params);
    void splitClipsAtSilences(const muse::actions::ActionData& args);
    void splitRangeSelectionAtSilences(const muse::actions::ActionData& args);
    void splitRangeSelectionIntoNewTracks(const muse::actions::ActionData& args);
    void splitClipsIntoNewTracks(const muse::actions::ActionData& args);
    void mergeSelectedOnTrack(const muse::actions::ActionData& args);
    void duplicateSelected(const muse::actions::ActionData& args);
    void duplicateClips(const muse::actions::ActionData& args);
    void splitCutSelected(const muse::actions::ActionData& args);
    void splitDeleteSelected(const muse::actions::ActionData& args);

    void deleteTracks(const muse::actions::ActionData&);
    muse::Ret duplicateTracks();

    muse::Ret moveTracksUp();
    muse::Ret moveTracksDown();
    muse::Ret moveTracksToTop();
    muse::Ret moveTracksToBottom();

    muse::Ret trimAudioOutsideSelection();
    muse::Ret doGlobalSilence();
    muse::Ret silenceAudioSelection();

    muse::Ret openClipPitchAndSpeed();

    muse::Ret swapStereoChannels();
    muse::Ret splitStereoToLR();
    muse::Ret splitStereoToCenter();
    muse::Ret setCustomTrackRate();
    muse::Ret makeStereoTrack();
    muse::Ret resampleTracks();

    muse::Ret groupClips();
    muse::Ret ungroupClips();

    muse::Ret selectNone();
    muse::Ret selectAllTracks();
    muse::Ret selectLeftOfPlaybackPos();
    muse::Ret selectRightOfPlaybackPos();
    muse::Ret selectTrackStartToCursor();
    muse::Ret selectCursorToTrackEnd();
    muse::Ret selectTrackStartToEnd();
    muse::Ret setSelection(const muse::rcommand::Params& params);
    muse::Ret selectTrackByIndex(const muse::rcommand::Params& params);
    muse::Ret moveCursorToClosestZeroCrossing();

    muse::Ret setClipColor(const muse::rcommand::Params& params, const std::function<void()>& onSet);
    muse::Ret setTrackColor(const muse::rcommand::Params& params);
    muse::Ret setTrackFormat(const muse::rcommand::Params& params);
    muse::Ret setTrackRate(const muse::rcommand::Params& params);

    muse::Ret toggleGlobalSpectrogramView();
    muse::Ret changeTrackView(const TrackId& trackId, TrackViewType trackView);

    muse::Ret addLabel();
    muse::Ret renameSelectedItem();

    void labelDeleteMulti(const muse::actions::ActionData& args);
    void labelCutMulti(const muse::actions::ActionData& args);
    void labelCopyMulti();

    void moveFocusedItem(secs_t timePositionOffset, int trackPositionOffset);
    muse::Ret extendFocusedItemBoundaryLeft();
    muse::Ret extendFocusedItemBoundaryRight();
    muse::Ret reduceFocusedItemBoundaryLeft();
    muse::Ret reduceFocusedItemBoundaryRight();

    double zoomLevel() const;
    double calculateStepSize() const;
    TrackId resolvePreviousTrackIdForMove(const TrackId& trackId) const;
    TrackId resolveNextTrackIdForMove(const TrackId& trackId) const;

    context::IPlaybackStatePtr playbackState() const;

    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    DeleteBehaviorOnboardingScenario m_deleteBehaviorOnboardingScenario;
};
}
