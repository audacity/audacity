/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "projectscene/view/tracksitemsview/selectionviewcontroller.h"
#include "projectscene/view/timeline/timelinecontext.h"

#include "snaptestaccess.h"

#include "global/tests/mocks/applicationmock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "spectrogram/tests/mocks/globalspectrogramconfigurationmock.h"
#include "spectrogram/tests/mocks/frequencyselectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "mocks/audiooutputmock.h"
#include "mocks/projectviewstatemock.h"

using namespace ::testing;

namespace au::projectscene {
//! The timeline context is left at its default zoom, so one second is one pixel.
//! Two tracks sit at the top of the view and end at 100px: track 1 spans 0..50px, track 2 spans 50..100px.
class SelectionBelowTracksTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_spectrogramConfiguration = std::make_shared<NiceMock<spectrogram::GlobalSpectrogramConfigurationMock> >();
        m_frequencySelectionController = std::make_shared<NiceMock<spectrogram::FrequencySelectionControllerMock> >();
        m_application = std::make_shared<NiceMock<muse::ApplicationMock> >();
        m_viewState = std::make_shared<NiceMock<ProjectViewStateMock> >();
        m_playback = std::make_shared<NiceMock<playback::PlaybackMock> >();
        m_audioOutput = std::make_shared<NiceMock<playback::AudioOutputMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));
        ON_CALL(*m_project, viewState())
        .WillByDefault(Return(m_viewState));
        ON_CALL(*m_project, trackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_playback, audioOutput())
        .WillByDefault(Return(m_audioOutput));
        ON_CALL(*m_application, keyboardModifiers())
        .WillByDefault(Return(Qt::NoModifier));

        ON_CALL(*m_trackeditProject, trackIdList())
        .WillByDefault(Return(trackedit::TrackIdList { 1, 2 }));
        ON_CALL(*m_viewState, isBelowAllTracks(_))
        .WillByDefault([](double y) { return y >= 100.0; });

        //! Any track selection and focus change is allowed unless a test expects a specific one
        EXPECT_CALL(*m_selectionController, setSelectedTracks(_, _)).Times(AnyNumber());
        EXPECT_CALL(*m_trackNavigationController, setFocus(_, _)).Times(AnyNumber());

        m_context = new TimelineContext();
        SnapTestAccess::wireContext(m_context, m_globalContext, m_playback);

        m_controller = new SelectionViewController();
        SnapTestAccess::wireSelection(m_controller, m_globalContext, m_selectionController, m_trackNavigationController,
                                      m_spectrogramConfiguration, m_frequencySelectionController, m_application);
        m_controller->setTimelineContext(m_context);
    }

    void TearDown() override
    {
        delete m_controller;
        delete m_context;
    }

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavigationController;
    std::shared_ptr<NiceMock<spectrogram::GlobalSpectrogramConfigurationMock> > m_spectrogramConfiguration;
    std::shared_ptr<NiceMock<spectrogram::FrequencySelectionControllerMock> > m_frequencySelectionController;
    std::shared_ptr<NiceMock<muse::ApplicationMock> > m_application;
    std::shared_ptr<NiceMock<ProjectViewStateMock> > m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::shared_ptr<NiceMock<context::PlaybackStateMock> > m_playbackState;

    TimelineContext* m_context = nullptr;
    SelectionViewController* m_controller = nullptr;
};

TEST_F(SelectionBelowTracksTests, DragBelowTheTracksSelectsTheTimeRangeOnAllTracks)
{
    //! [EXPECT] While and after dragging, all tracks are selected, and the bottom one takes the focus
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, false)).Times(AtLeast(1));
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, true)).Times(1);
    EXPECT_CALL(*m_trackNavigationController, setFocus(trackedit::TrackFocus::track(2), _)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(trackedit::secs_t(1.0), false)).Times(AtLeast(1));
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(trackedit::secs_t(20.0), false)).Times(AtLeast(1));
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(trackedit::secs_t(1.0), true)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(trackedit::secs_t(20.0), true)).Times(1);

    //! [WHEN] A drag starts and ends in the empty area below the tracks
    m_controller->onPressed(1.0, 120.0);
    m_controller->onPositionChanged(20.0, 130.0);
    m_controller->onReleased(20.0, 130.0);
}

TEST_F(SelectionBelowTracksTests, DragFromBelowAcrossTracksSelectsOnlyTheCrossedTracks)
{
    //! [GIVEN] Dragged up from below the tracks into track 2
    ON_CALL(*m_viewState, tracksInRange(120.0, 60.0))
    .WillByDefault(Return(trackedit::TrackIdList { 2 }));

    //! [EXPECT] Only track 2 is selected, never all tracks
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, _)).Times(0);
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 2 }, false)).Times(AtLeast(1));
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 2 }, true)).Times(1);

    //! [WHEN] The drag ends on track 2
    m_controller->onPressed(1.0, 120.0);
    m_controller->onPositionChanged(20.0, 60.0);
    m_controller->onReleased(20.0, 60.0);
}

TEST_F(SelectionBelowTracksTests, DragBackBelowTheTracksSelectsAllTracksAgain)
{
    //! [GIVEN] A drag from below the tracks that went up into track 2
    ON_CALL(*m_viewState, tracksInRange(120.0, 60.0))
    .WillByDefault(Return(trackedit::TrackIdList { 2 }));
    m_controller->onPressed(1.0, 120.0);
    m_controller->onPositionChanged(20.0, 60.0);

    //! [EXPECT] All tracks are covered again
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, false)).Times(1);

    //! [WHEN] The pointer returns to the empty area below the tracks
    m_controller->onPositionChanged(25.0, 130.0);
}

TEST_F(SelectionBelowTracksTests, ClickBelowTheTracksDeselectsThem)
{
    //! [EXPECT] A click without a drag selects no tracks and leaves only a cursor in time
    EXPECT_CALL(*m_selectionController, setSelectedTracks(trackedit::TrackIdList { 1, 2 }, _)).Times(0);
    EXPECT_CALL(*m_selectionController, resetSelectedTracks()).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(trackedit::secs_t(1.0), true)).Times(1);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(trackedit::secs_t(1.0), true)).Times(1);

    //! [WHEN] The empty area below the tracks is clicked
    m_controller->onPressed(1.0, 120.0);
    m_controller->onReleased(1.0, 120.0);
}
}
