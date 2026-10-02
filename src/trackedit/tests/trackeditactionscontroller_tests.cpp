/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/internal/trackeditactionscontroller.h"
#include "trackedit/internal/tracksviewrequestsservice.h"

#include "mocks/selectioncontrollermock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditinteractionmock.h"
#include "mocks/projecthistorymock.h"
#include "mocks/trackeditprojectmock.h"
#include "actions/tests/mocks/actionsdispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"

using ::testing::NiceMock;
using ::testing::Return;

namespace au::trackedit {
class TrackeditActionsControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<TrackNavigationControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<TrackeditInteractionMock> >();
        m_projectHistory = std::make_shared<NiceMock<ProjectHistoryMock> >();

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackeditActionsController>(m_testCtx);

        m_controller->selectionController.set(m_selectionController);
        m_controller->trackNavigationController.set(m_trackNavigationController);
        m_controller->trackeditInteraction.set(m_trackeditInteraction);
        m_controller->projectHistory.set(m_projectHistory);
        m_requests = std::make_shared<TracksViewRequestsService>(m_testCtx);
        m_controller->tracksViewRequestsService.set(m_requests);
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_controller->dispatcher.set(m_dispatcher);

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        m_controller->globalContext.set(m_globalContext);
        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_project, trackeditProject()).WillByDefault(Return(m_trackeditProject));

        ON_CALL(*m_trackNavigationController, focus())
        .WillByDefault(Return(TrackFocus::track(INVALID_TRACK)));

        ON_CALL(*m_selectionController, selectedClips())
        .WillByDefault(Return(ClipKeyList {}));
        ON_CALL(*m_selectionController, selectedClipsInTrackOrder())
        .WillByDefault(Return(ClipKeyList {}));
        ON_CALL(*m_selectionController, selectedLabels())
        .WillByDefault(Return(LabelKeyList {}));
        ON_CALL(*m_selectionController, selectedLabelsInTrackOrder())
        .WillByDefault(Return(LabelKeyList {}));
        ON_CALL(*m_selectionController, selectedTracks())
        .WillByDefault(Return(TrackIdList {}));
        ON_CALL(*m_selectionController, timeSelectionIsEmpty())
        .WillByDefault(Return(true));
    }

    void TearDown() override
    {
        m_controller.reset();
        muse::modularity::removeIoC(m_testCtx);
    }

    void cancel()
    {
        m_controller->doGlobalCancel();
    }

    void moveItem(secs_t timeOffset, int trackOffset)
    {
        m_controller->moveFocusedItem(timeOffset, trackOffset);
    }

    void copyMultiItems()
    {
        m_controller->multiClipCopy();
    }

    void cutMultiItems(bool moveClips)
    {
        m_controller->multiClipCut(muse::actions::ActionData::make_arg1<bool>(moveClips));
    }

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackeditActionsController> m_controller;

    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<TracksViewRequestsService> m_requests;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_project;
    std::shared_ptr<TrackeditProjectMock> m_trackeditProject;
};

TEST_F(TrackeditActionsControllerTests, HistoryEventsRefreshTheGroupActions)
{
    //! [GIVEN] A controller listening to the history, whose events can restore group ids without touching the selection
    muse::async::Channel<HistoryEvent> historyChanged;
    ON_CALL(*m_projectHistory, historyChanged()).WillByDefault(Return(historyChanged));
    m_controller->init();
    std::vector<muse::actions::ActionCode> refreshed;
    m_controller->actionEnabledChanged().onReceive(m_controller.get(), [&refreshed](const muse::actions::ActionCode& code) {
        refreshed.push_back(code);
    });

    //! [WHEN] The history reports a restored state
    historyChanged.send(HistoryEvent::RestoredState);

    //! [THEN] Both group actions are re-evaluated
    EXPECT_TRUE(muse::contains(refreshed, muse::actions::ActionCode("group-items")));
    EXPECT_TRUE(muse::contains(refreshed, muse::actions::ActionCode("ungroup-items")));
}

TEST_F(TrackeditActionsControllerTests, UngroupIsAvailableForASingleGroupedItem)
{
    //! [GIVEN] One selected clip that still carries a group id, the rest of its group having been removed
    const ClipKey clipKey { 1, 10 };
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList { clipKey }));
    ON_CALL(*m_trackeditInteraction, itemGroupId(clipKey)).WillByDefault(Return(int64_t(7)));

    //! [THEN] Ungroup can clear that id, while Group needs more than one item
    EXPECT_TRUE(m_controller->canReceiveAction("ungroup-items"));
    EXPECT_FALSE(m_controller->canReceiveAction("group-items"));

    //! [WHEN] The clip is not grouped
    ON_CALL(*m_trackeditInteraction, itemGroupId(clipKey)).WillByDefault(Return(int64_t(-1)));

    //! [THEN] Neither action applies to it alone
    EXPECT_FALSE(m_controller->canReceiveAction("ungroup-items"));
    EXPECT_FALSE(m_controller->canReceiveAction("group-items"));
}

TEST_F(TrackeditActionsControllerTests, MultiClipCopyHandsClipsAndLabelsToOneCopy)
{
    //! [GIVEN] A clip and a label are selected
    const ClipKey clipKey { 1, 10 };
    const LabelKey labelKey { 2, 20 };
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList { clipKey }));
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(LabelKeyList { labelKey }));

    //! [EXPECT] Both go to the copy operation together
    EXPECT_CALL(*m_trackeditInteraction, copyItems(ClipKeyList { clipKey }, LabelKeyList { labelKey })).Times(1);

    //! [WHEN] The multi-item copy runs
    copyMultiItems();
}

TEST_F(TrackeditActionsControllerTests, MultiClipCutHandsClipsAndLabelsToOneCut)
{
    //! [GIVEN] A clip and a label are selected
    const ClipKey clipKey { 1, 10 };
    const LabelKey labelKey { 2, 20 };
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList { clipKey }));
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(LabelKeyList { labelKey }));

    //! [EXPECT] Both go to the cut operation together, which owns the copy and the removal
    EXPECT_CALL(*m_trackeditInteraction, cutItems(ClipKeyList { clipKey }, LabelKeyList { labelKey }, false)).Times(1);
    EXPECT_CALL(*m_trackeditInteraction, removeClips(::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_trackeditInteraction, removeLabels(::testing::_, ::testing::_)).Times(0);

    //! [WHEN] The multi-item cut runs
    cutMultiItems(false);
}

TEST_F(TrackeditActionsControllerTests, KeyboardMoveRequestsPreviewInsteadOfEditingItems)
{
    std::vector<std::pair<secs_t, int> > steps;
    m_requests->itemMoveRequested().onReceive(m_controller.get(), [&steps](secs_t timeOffset, int trackOffset) {
        steps.emplace_back(timeOffset, trackOffset);
    });
    EXPECT_CALL(*m_trackeditInteraction, moveClips(::testing::_, ::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_trackeditInteraction, moveLabels(::testing::_, ::testing::_, ::testing::_)).Times(0);

    moveItem(0.5, 0);
    moveItem(0.0, 1);
    const std::vector<std::pair<secs_t, int> > expected { { 0.5, 0 }, { 0.0, 1 } };
    EXPECT_EQ(steps, expected);
}

/**
 * Cancel always notifies about the in-progress drag edit being cancelled.
 */
TEST_F(TrackeditActionsControllerTests, AlwaysNotifiesCancelDragEdit)
{
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).Times(1);

    cancel();
}

TEST_F(TrackeditActionsControllerTests, CancelDragPreservesTimeSelectionUntilNextEscape)
{
    bool interactionOngoing = true;
    ON_CALL(*m_projectHistory, interactionOngoing())
    .WillByDefault([&interactionOngoing] { return interactionOngoing; });
    ON_CALL(*m_selectionController, timeSelectionIsEmpty()).WillByDefault(Return(false));
    ON_CALL(*m_selectionController, selectedTracks()).WillByDefault(Return(TrackIdList { 5 }));

    ::testing::InSequence sequence;
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).WillOnce([&interactionOngoing] {
        interactionOngoing = false;
    });
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit());
    EXPECT_CALL(*m_selectionController, resetTimeSelection());
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(5), false));

    cancel();
    cancel();
}

/**
 * [Stage 1] A clip is focused with no selection: the focus is dropped and moved onto the
 * clip's own track.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusNoSelection_DropsFocusAndFocusesTrack)
{
    //! [GIVEN] A clip is focused on track 1, nothing is selected
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::item({ 1, 100 })));

    //! [EXPECT] The focus is dropped and moved to the focused clip's track (no item focus)
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(1), false)).Times(1);

    cancel();
}

/**
 * [Stage 1] A clip is focused while clips are selected: the focus is dropped and moved onto
 * the first selected clip.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusWithClipSelection_MovesFocusToSelectedClip)
{
    //! [GIVEN] A clip is focused, and clip {2, 200} is selected
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::item({ 1, 100 })));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The focus is dropped and moved onto the selected clip
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::item({ 2, 200 }), false)).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(0);

    cancel();
}

/**
 * [Stage 1] A clip is focused while only labels are selected: the focus is dropped and moved
 * onto the first selected label.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusWithLabelSelection_MovesFocusToSelectedLabel)
{
    //! [GIVEN] A clip is focused, no clips selected, label {3, 300} is selected
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::item({ 1, 100 })));
    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));

    //! [EXPECT] The focus is dropped and moved onto the selected label
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::item({ 3, 300 }), false)).Times(1);

    cancel();
}

/**
 * [Stage 1] The focused clip is itself part of the clip selection: the focus is dropped, the clip
 * selection is cleared and the focus falls back to the current track.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusOnSelectedClip_DeselectsAndFocusesTrack)
{
    //! [GIVEN] Clip {2, 200} is focused and is part of the clip selection
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::item({ 2, 200 })));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The focus is dropped, the clip selection cleared and the clip's track focused
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(2), false)).Times(1);

    cancel();
}

/**
 * [Stage 1] The focused label is itself part of the label selection: the focus is dropped, the
 * label selection is cleared and the focus falls back to the current track.
 */
TEST_F(TrackeditActionsControllerTests, LabelFocusOnSelectedLabel_DeselectsAndFocusesTrack)
{
    //! [GIVEN] Label {3, 300} is focused and is part of the label selection (no clips selected)
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::item({ 3, 300 })));
    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));

    //! [EXPECT] The focus is dropped, the label selection cleared and the label's track focused
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedLabels()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(3), false)).Times(1);

    cancel();
}

/**
 * A track (no item) is focused with no selection: Escape only cancels the drag edit; there is
 * no item focus to drop and no selection to reset.
 */
TEST_F(TrackeditActionsControllerTests, TrackFocusNoSelection_OnlyCancelsDragEdit)
{
    //! [GIVEN] A track (no item) is focused, nothing is selected
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::track(1)));

    //! [EXPECT] Only the drag edit is cancelled
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocus(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * [Stage 2] A track is focused while clips are selected: the clip selection is reset and the
 * focus stays on the current track.
 */
TEST_F(TrackeditActionsControllerTests, TrackFocusWithClipSelection_ResetsSelectionAndFocusesTrack)
{
    //! [GIVEN] Track 1 is focused (no item), and clip {2, 200} is selected
    ON_CALL(*m_trackNavigationController, focus())
    .WillByDefault(Return(TrackFocus::track(1)));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The clip selection is reset and the current (focused) track is focused
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(1), false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(0);

    cancel();
}

/**
 * [Stage 2] Clips are selected with nothing focused: the clip selection is reset and the focus
 * moves to the selected clip's track.
 */
TEST_F(TrackeditActionsControllerTests, ClipSelectionNoFocus_ResetsSelectionAndFocusesClipTrack)
{
    //! [GIVEN] Nothing focused, clip {2, 200} selected, no track selected
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));
    ON_CALL(*m_selectionController, selectedClipsInTrackOrder())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The clip selection is reset and the clip's track is focused
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(2), false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(0);

    cancel();
}

/**
 * [Stage 2] Labels are selected with nothing focused: the label selection is reset and the focus
 * moves to the selected label's track.
 */
TEST_F(TrackeditActionsControllerTests, LabelSelectionNoFocus_ResetsSelectionAndFocusesLabelTrack)
{
    //! [GIVEN] Nothing focused, no clips selected, label {3, 300} selected
    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));
    ON_CALL(*m_selectionController, selectedLabelsInTrackOrder())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));

    //! [EXPECT] The label selection is reset and the label's track is focused
    EXPECT_CALL(*m_selectionController, resetSelectedLabels()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(3), false)).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(0);

    cancel();
}

/**
 * [Stage 2] A data range (time) selection with nothing focused: the time selection is reset and
 * the focus moves to the selected track.
 */
TEST_F(TrackeditActionsControllerTests, TimeSelectionNoFocus_ResetsTimeSelectionAndFocusesTrack)
{
    //! [GIVEN] Nothing focused, no clips/labels, a time selection on track 5
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { 5 }));

    //! [EXPECT] The time selection is reset and the selected track is focused
    EXPECT_CALL(*m_selectionController, resetTimeSelection()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(TrackFocus::track(5), false)).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(0);
    EXPECT_CALL(*m_selectionController, resetSelectedLabels()).Times(0);

    cancel();
}

/**
 * Nothing is focused and nothing is selected: Escape only cancels the drag edit.
 */
TEST_F(TrackeditActionsControllerTests, NoFocusNoSelection_OnlyCancelsDragEdit)
{
    //! [EXPECT] Only the drag edit is cancelled, nothing else changes
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocus(::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_selectionController, resetTimeSelection()).Times(0);

    cancel();
}
}
