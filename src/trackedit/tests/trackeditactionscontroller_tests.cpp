/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/internal/trackeditactionscontroller.h"
#include "trackedit/internal/tracksviewrequestsservice.h"

#include "mocks/selectioncontrollermock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditinteractionmock.h"
#include "mocks/trackeditprojectmock.h"
#include "mocks/projecthistorymock.h"
#include "context/tests/mocks/globalcontextmock.h"

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
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_trackeditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        m_controller = std::make_shared<TrackeditActionsController>(m_testCtx);

        m_controller->selectionController.set(m_selectionController);
        m_controller->trackNavigationController.set(m_trackNavigationController);
        m_controller->trackeditInteraction.set(m_trackeditInteraction);
        m_controller->projectHistory.set(m_projectHistory);
        m_controller->globalContext.set(m_globalContext);
        m_requests = std::make_shared<TracksViewRequestsService>(m_testCtx);
        m_controller->tracksViewRequestsService.set(m_requests);

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

    muse::Ret mixAndRender(bool toNewTrack)
    {
        return m_controller->mixAndRender(toNewTrack);
    }

    void addTrack(TrackId trackId, TrackType type)
    {
        Track track;
        track.id = trackId;
        track.type = type;
        ON_CALL(*m_trackeditProject, track(trackId))
        .WillByDefault(Return(track));
    }

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackeditActionsController> m_controller;

    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<TracksViewRequestsService> m_requests;
};

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

TEST_F(TrackeditActionsControllerTests, MixAndRenderMixesOnlyTheSelectedWaveTracks)
{
    //! [GIVEN] A mono, a label and a stereo track are selected
    addTrack(1, TrackType::Mono);
    addTrack(2, TrackType::Label);
    addTrack(3, TrackType::Stereo);
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { 1, 2, 3 }));

    //! [EXPECT] Only the wave tracks are mixed, replacing them
    EXPECT_CALL(*m_trackeditInteraction, mixAndRender(TrackIdList { 1, 3 }, false))
    .WillOnce(Return(muse::RetVal<TrackId>::make_ok(4)));

    //! [WHEN] Mix and render is triggered
    EXPECT_TRUE(mixAndRender(false));
}

TEST_F(TrackeditActionsControllerTests, MixAndRenderToNewTrackKeepsTheOriginals)
{
    //! [GIVEN] A mono track is selected
    addTrack(1, TrackType::Mono);
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { 1 }));

    //! [EXPECT] The mix goes to a new track
    EXPECT_CALL(*m_trackeditInteraction, mixAndRender(TrackIdList { 1 }, true))
    .WillOnce(Return(muse::RetVal<TrackId>::make_ok(2)));

    //! [WHEN] Mix and render to new track is triggered
    EXPECT_TRUE(mixAndRender(true));
}

TEST_F(TrackeditActionsControllerTests, MixAndRenderWithNothingSelectedMixesEveryAudioTrack)
{
    //! [GIVEN] No track is selected in a project with a mono, a label and a stereo track
    addTrack(1, TrackType::Mono);
    addTrack(2, TrackType::Label);
    addTrack(3, TrackType::Stereo);
    ON_CALL(*m_trackeditProject, trackIdList())
    .WillByDefault(Return(TrackIdList { 1, 2, 3 }));

    //! [EXPECT] Every audio track is mixed
    EXPECT_CALL(*m_trackeditInteraction, mixAndRender(TrackIdList { 1, 3 }, false))
    .WillOnce(Return(muse::RetVal<TrackId>::make_ok(4)));

    //! [WHEN] Mix and render is triggered
    EXPECT_TRUE(mixAndRender(false));
}

TEST_F(TrackeditActionsControllerTests, MixAndRenderWithoutSelectedWaveTracksDoesNothing)
{
    //! [GIVEN] Only a label track is selected
    addTrack(1, TrackType::Label);
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { 1 }));

    //! [EXPECT] Nothing is mixed
    EXPECT_CALL(*m_trackeditInteraction, mixAndRender(::testing::_, ::testing::_)).Times(0);

    //! [WHEN] Mix and render is triggered
    EXPECT_FALSE(mixAndRender(false));
}
}
