/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <map>

#include "projectscene/internal/trackheightcontroller.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "mocks/projectviewstatemock.h"

using namespace ::testing;

namespace au::projectscene {
namespace {
constexpr int VIEWPORT_HEIGHT = 600;
constexpr int MIN_HEIGHT = 44;
constexpr int DEFAULT_HEIGHT = 114;
}

class TrackHeightControllerTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_viewState = std::make_shared<NiceMock<ProjectViewStateMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();

        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_project, viewState()).WillByDefault(Return(m_viewState));
        ON_CALL(*m_trackeditProject, trackIdList()).WillByDefault(Return(trackedit::TrackIdList { 1, 2, 3, 4 }));
        ON_CALL(*m_trackNavigationController, focusedTrack()).WillByDefault(Return(trackedit::INVALID_TRACK));
        ON_CALL(*m_viewState, tracksViewportHeight()).WillByDefault(Return(VIEWPORT_HEIGHT));
        ON_CALL(*m_viewState, trackMinHeight()).WillByDefault(Return(MIN_HEIGHT));
        ON_CALL(*m_viewState, trackDefaultHeight(_)).WillByDefault(Return(DEFAULT_HEIGHT));
        ON_CALL(*m_viewState, setTrackHeight(_, _)).WillByDefault(Invoke([this](const trackedit::TrackId& trackId, int height) {
            m_heights[trackId] = height;
        }));
        ON_CALL(*m_viewState, collapseTrack(_)).WillByDefault(Invoke([this](const trackedit::TrackId& trackId) {
            m_heights[trackId] = MIN_HEIGHT;
        }));
        ON_CALL(*m_viewState, trackHeight(_)).WillByDefault(Invoke([this](const trackedit::TrackId& trackId) {
            muse::ValCh<int> height;
            height.val = m_heights.contains(trackId) ? m_heights.at(trackId) : DEFAULT_HEIGHT;
            return height;
        }));

        m_controller = std::make_shared<TrackHeightController>(muse::modularity::globalCtx());
        m_controller->globalContext.set(m_globalContext);
        m_controller->selectionController.set(m_selectionController);
        m_controller->trackNavigationController.set(m_trackNavigationController);
    }

    void focus(trackedit::TrackId trackId)
    {
        ON_CALL(*m_trackNavigationController, focusedTrack()).WillByDefault(Return(trackId));
    }

    void select(const trackedit::TrackIdList& tracks)
    {
        ON_CALL(*m_selectionController, selectedTracks()).WillByDefault(Return(tracks));
    }

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<NiceMock<ProjectViewStateMock> > m_viewState;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavigationController;
    std::shared_ptr<TrackHeightController> m_controller;
    std::map<trackedit::TrackId, int> m_heights;
};

TEST_F(TrackHeightControllerTests, TargetsSelectedTracksWhenTheFocusedOneIsSelected)
{
    //! [GIVEN] Tracks 2 and 3 selected, track 3 focused
    select({ 2, 3 });
    focus(3);

    //! [THEN] Both selected tracks are affected
    EXPECT_EQ(m_controller->targetTracks(), (trackedit::TrackIdList { 2, 3 }));
}

TEST_F(TrackHeightControllerTests, TargetsOnlyTheFocusedTrackWhenItIsNotSelected)
{
    //! [GIVEN] Tracks 2 and 3 selected, track 4 focused
    select({ 2, 3 });
    focus(4);

    //! [THEN] Only the focused track is affected
    EXPECT_EQ(m_controller->targetTracks(), (trackedit::TrackIdList { 4 }));
}

TEST_F(TrackHeightControllerTests, TargetsSelectedTracksWithoutFocus)
{
    //! [GIVEN] Track 2 selected and no track focused
    select({ 2 });

    //! [THEN] The selected track is affected
    EXPECT_EQ(m_controller->targetTracks(), (trackedit::TrackIdList { 2 }));
}

TEST_F(TrackHeightControllerTests, TargetsTheGivenTrackRatherThanTheFocusedOne)
{
    //! [GIVEN] Tracks 2 and 3 selected, track 3 focused
    select({ 2, 3 });
    focus(3);

    //! [THEN] An action on track 1, outside the selection, affects only that track
    EXPECT_EQ(m_controller->targetTracks(1), (trackedit::TrackIdList { 1 }));
    //! [THEN] An action on selected track 2 affects the whole selection
    EXPECT_EQ(m_controller->targetTracks(2), (trackedit::TrackIdList { 2, 3 }));
}

TEST_F(TrackHeightControllerTests, TargetsTheFocusedTrackWhenTheGivenOneIsNotInTheProject)
{
    //! [GIVEN] Track 3 is focused and not selected
    focus(3);
    select({ 1 });

    //! [THEN] An unknown track falls back to the focused one
    EXPECT_EQ(m_controller->targetTracks(99), (trackedit::TrackIdList { 3 }));
}

TEST_F(TrackHeightControllerTests, ChangeHeightResizesEachTrack)
{
    //! [EXPECT] Each track gets 24 px taller
    EXPECT_CALL(*m_viewState, changeTrackHeight(1, 24));
    EXPECT_CALL(*m_viewState, changeTrackHeight(3, 24));

    //! [WHEN] Tracks 1 and 3 are made taller
    m_controller->changeHeight({ 1, 3 }, 24);
}

TEST_F(TrackHeightControllerTests, ResetSetsTheDefaultHeight)
{
    //! [WHEN] Track 2 is reset
    m_controller->reset({ 2 });

    //! [THEN] It gets its default height
    EXPECT_EQ(m_heights[2], DEFAULT_HEIGHT);
}

TEST_F(TrackHeightControllerTests, FitAllTracksSharesTheViewHeight)
{
    //! [WHEN] All four tracks are fitted to the 600 px view
    m_controller->fitToHeight(m_controller->allTracks());

    //! [THEN] Each one gets a quarter of it
    EXPECT_EQ(m_heights, (std::map<trackedit::TrackId, int> { { 1, 150 }, { 2, 150 }, { 3, 150 }, { 4, 150 } }));
}

TEST_F(TrackHeightControllerTests, FitFillsTheViewExactly)
{
    //! [GIVEN] A 601 px view
    ON_CALL(*m_viewState, tracksViewportHeight()).WillByDefault(Return(601));

    //! [WHEN] Two tracks are fitted
    m_controller->fitToHeight({ 1, 2 });

    //! [THEN] They fill what the two collapsed tracks leave, and the leftover pixel goes to the first one
    EXPECT_EQ(m_heights[1] + m_heights[2] + m_heights[3] + m_heights[4], 601);
    EXPECT_EQ(m_heights[1], 257);
}

TEST_F(TrackHeightControllerTests, FitSomeTracksCollapsesTheOthers)
{
    //! [EXPECT] The tracks that aren't fitted collapse
    EXPECT_CALL(*m_viewState, collapseTrack(1));
    EXPECT_CALL(*m_viewState, collapseTrack(4));
    EXPECT_CALL(*m_viewState, collapseTrack(2)).Times(0);
    EXPECT_CALL(*m_viewState, collapseTrack(3)).Times(0);

    //! [WHEN] Tracks 2 and 3 are fitted
    m_controller->fitToHeight({ 2, 3 });

    //! [THEN] They share the view left by the collapsed tracks
    EXPECT_EQ(m_heights[1], MIN_HEIGHT);
    EXPECT_EQ(m_heights[4], MIN_HEIGHT);
    EXPECT_EQ(m_heights[2], 256);
    EXPECT_EQ(m_heights[3], 256);
}

TEST_F(TrackHeightControllerTests, FitScrollsToTheTopWhenAllTracksFit)
{
    //! [EXPECT] The view scrolls to the first track
    EXPECT_CALL(*m_viewState, changeTracksVerticalOffset(0));

    //! [WHEN] One track is fitted
    m_controller->fitToHeight({ 3 });

    //! [THEN] It takes what the three collapsed tracks leave
    EXPECT_EQ(m_heights[3], VIEWPORT_HEIGHT - 3 * MIN_HEIGHT);
}

TEST_F(TrackHeightControllerTests, FitScrollsToTheFirstFittedTrackWhenTheTracksDontFit)
{
    //! [GIVEN] A view too short for the collapsed and the fitted tracks
    ON_CALL(*m_viewState, tracksViewportHeight()).WillByDefault(Return(100));

    //! [GIVEN] The view is scrolled by 100 px and track 2 starts 44 px below its top
    muse::ValCh<int> offset;
    offset.val = 100;
    ON_CALL(*m_viewState, tracksVerticalOffset()).WillByDefault(Return(offset));
    ON_CALL(*m_viewState, trackVerticalPosition(2)).WillByDefault(Return(44));
    ON_CALL(*m_viewState, trackVerticalPosition(3)).WillByDefault(Return(344));

    //! [EXPECT] The view scrolls so that track 2, the upper of the fitted tracks, is at its top
    EXPECT_CALL(*m_viewState, changeTracksVerticalOffset(144));

    //! [WHEN] Tracks 3 and 2 are fitted
    m_controller->fitToHeight({ 3, 2 });
}

TEST_F(TrackHeightControllerTests, FitKeepsTheMinimumHeight)
{
    //! [GIVEN] A view too short for all tracks
    ON_CALL(*m_viewState, tracksViewportHeight()).WillByDefault(Return(100));

    //! [WHEN] All tracks are fitted
    m_controller->fitToHeight(m_controller->allTracks());

    //! [THEN] None gets shorter than the minimum height
    for (const auto& [trackId, height] : m_heights) {
        EXPECT_EQ(height, MIN_HEIGHT) << trackId;
    }
}

TEST_F(TrackHeightControllerTests, FitDoesNothingWithoutAView)
{
    //! [GIVEN] The view height is not known yet
    ON_CALL(*m_viewState, tracksViewportHeight()).WillByDefault(Return(0));

    //! [EXPECT] No track changes
    EXPECT_CALL(*m_viewState, collapseTrack(_)).Times(0);
    EXPECT_CALL(*m_viewState, setTrackHeight(_, _)).Times(0);
    EXPECT_CALL(*m_viewState, changeTracksVerticalOffset(_)).Times(0);

    //! [WHEN] Tracks are fitted
    m_controller->fitToHeight({ 1 });
}
}
