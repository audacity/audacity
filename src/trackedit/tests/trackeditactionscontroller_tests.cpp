/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/internal/trackeditactionscontroller.h"

#include "mocks/selectioncontrollermock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditinteractionmock.h"

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

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackeditActionsController>(m_testCtx);

        m_controller->selectionController.set(m_selectionController);
        m_controller->trackNavigationController.set(m_trackNavigationController);
        m_controller->trackeditInteraction.set(m_trackeditInteraction);

        ON_CALL(*m_trackNavigationController, focusedItem())
        .WillByDefault(Return(TrackItemKey { INVALID_TRACK, INVALID_TRACK_ITEM }));

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

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackeditActionsController> m_controller;

    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
};

/**
 * Cancel always notifies about the in-progress drag edit being cancelled.
 */
TEST_F(TrackeditActionsControllerTests, AlwaysNotifiesCancelDragEdit)
{
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).Times(1);

    cancel();
}

/**
 * [Stage 1] A clip is focused with no selection: the focus is dropped and moved onto the
 * clip's own track.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusNoSelection_DropsFocusAndFocusesTrack)
{
    //! [GIVEN] A clip is focused on track 1, nothing is selected
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 1, 100 }));

    //! [EXPECT] The focus is dropped and moved to the focused clip's track (no item focus)
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(1, false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * [Stage 1] A clip is focused while clips are selected: the focus is dropped and moved onto
 * the first selected clip.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusWithClipSelection_MovesFocusToSelectedClip)
{
    //! [GIVEN] A clip is focused, and clip {2, 200} is selected
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 1, 100 }));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The focus is dropped and moved onto the selected clip
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(TrackItemKey { 2, 200 }, false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(::testing::_, ::testing::_)).Times(0);
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
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 1, 100 }));
    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));

    //! [EXPECT] The focus is dropped and moved onto the selected label
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(TrackItemKey { 3, 300 }, false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * [Stage 1] The focused clip is itself part of the clip selection: the focus is dropped, the clip
 * selection is cleared and the focus falls back to the current track.
 */
TEST_F(TrackeditActionsControllerTests, ClipFocusOnSelectedClip_DeselectsAndFocusesTrack)
{
    //! [GIVEN] Clip {2, 200} is focused and is part of the clip selection
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 2, 200 }));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The focus is dropped, the clip selection cleared and the clip's track focused
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(2, false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * [Stage 1] The focused label is itself part of the label selection: the focus is dropped, the
 * label selection is cleared and the focus falls back to the current track.
 */
TEST_F(TrackeditActionsControllerTests, LabelFocusOnSelectedLabel_DeselectsAndFocusesTrack)
{
    //! [GIVEN] Label {3, 300} is focused and is part of the label selection (no clips selected)
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 3, 300 }));
    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(LabelKeyList { { 3, 300 } }));

    //! [EXPECT] The focus is dropped, the label selection cleared and the label's track focused
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(1);
    EXPECT_CALL(*m_selectionController, resetSelectedLabels()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(3, false)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * A track (no item) is focused with no selection: Escape only cancels the drag edit; there is
 * no item focus to drop and no selection to reset.
 */
TEST_F(TrackeditActionsControllerTests, TrackFocusNoSelection_OnlyCancelsDragEdit)
{
    //! [GIVEN] A track (no item) is focused, nothing is selected
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 1, INVALID_TRACK_ITEM }));

    //! [EXPECT] Only the drag edit is cancelled
    EXPECT_CALL(*m_trackeditInteraction, notifyAboutCancelDragEdit()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, resetNavigation()).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(::testing::_, ::testing::_)).Times(0);

    cancel();
}

/**
 * [Stage 2] A track is focused while clips are selected: the clip selection is reset and the
 * focus stays on the current track.
 */
TEST_F(TrackeditActionsControllerTests, TrackFocusWithClipSelection_ResetsSelectionAndFocusesTrack)
{
    //! [GIVEN] Track 1 is focused (no item), and clip {2, 200} is selected
    ON_CALL(*m_trackNavigationController, focusedItem())
    .WillByDefault(Return(TrackItemKey { 1, INVALID_TRACK_ITEM }));
    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(ClipKeyList { { 2, 200 } }));

    //! [EXPECT] The clip selection is reset and the current (focused) track is focused
    EXPECT_CALL(*m_selectionController, resetSelectedClips()).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(1, false)).Times(1);
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
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(2, false)).Times(1);
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
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(3, false)).Times(1);
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
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(5, false)).Times(1);
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
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocusedItem(::testing::_, ::testing::_)).Times(0);
    EXPECT_CALL(*m_selectionController, resetTimeSelection()).Times(0);

    cancel();
}
}
