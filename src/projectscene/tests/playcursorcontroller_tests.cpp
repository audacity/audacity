/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <set>

#include "projectscene/view/playcursor/playcursorcontroller.h"
#include "projectscene/view/timeline/timelinecontext.h"
#include "projectscene/internal/projectviewstate.h"

#include "snaptestaccess.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/audiooutputmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "actions/tests/mocks/actionsdispatchermock.h"

using namespace ::testing;

namespace au::projectscene {
//! Exercises PlayCursorController's seek/play-region dispatch, focusing on the
//! play-cursor snapping: the controller must ask the
//! TimelineContext to snap (withSnap = true) before dispatching.
//! Default zoom (1.0) / frame start (0.0) make position and time 1:1.
class PlayCursorControllerTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_viewState = std::make_shared<ProjectViewState>(muse::modularity::globalCtx());
        m_playback = std::make_shared<NiceMock<playback::PlaybackMock> >();
        m_audioOutput = std::make_shared<NiceMock<playback::AudioOutputMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));
        ON_CALL(*m_project, viewState())
        .WillByDefault(Return(m_viewState));
        ON_CALL(*m_playback, audioOutput())
        .WillByDefault(Return(m_audioOutput));
        ON_CALL(*m_audioOutput, sampleRate())
        .WillByDefault(Return(static_cast<uint64_t>(44100)));
        ON_CALL(*m_trackeditProject, timeSignature())
        .WillByDefault(Return(trackedit::TimeSignature { 120.0, 4, 4 }));
        ON_CALL(*m_playbackState, isPlaying())
        .WillByDefault(Return(false));
        ON_CALL(*m_playbackState, playbackPosition())
        .WillByDefault(Return(muse::secs_t(0.0)));

        m_context = new TimelineContext();
        SnapTestAccess::wireContext(m_context, m_globalContext, m_playback);

        m_controller = new PlayCursorController();
        SnapTestAccess::wireCursor(m_controller, m_globalContext, m_dispatcher);
        m_controller->setTimelineContext(m_context);
    }

    void TearDown() override
    {
        delete m_controller;
        delete m_context;
    }

    void useGridSnapOff(const std::set<muse::secs_t>& boundaries)
    {
        m_viewState->setIsSnapEnabled(false);
        m_viewState->setItemsBoundaries(boundaries);
    }

    void useGridSnapOn(SnapType type)
    {
        m_viewState->setIsSnapEnabled(true);
        m_viewState->setSnapType(type);
    }

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<ProjectViewState> m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::shared_ptr<NiceMock<context::PlaybackStateMock> > m_playbackState;
    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > m_dispatcher;

    TimelineContext* m_context = nullptr;
    PlayCursorController* m_controller = nullptr;
};

TEST_F(PlayCursorControllerTests, SeekToX_GridSnapOff_SnapsToBoundary)
{
    //! CASE Seeking near an item boundary dispatches a seek at the snapped boundary.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->seekToX(10.5); // within the 4px(=4s at zoom 1) clip-snap tolerance

    EXPECT_TRUE(captured.contains("seekTime"));
    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 10.0);
    EXPECT_FALSE(captured.param("triggerPlay").toBool());
}

TEST_F(PlayCursorControllerTests, SeekToX_GridSnapOff_FarFromBoundary_UsesRawTime)
{
    //! CASE Outside the snap tolerance, the raw time is used.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->seekToX(20.0); // 10s away from the only boundary

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 20.0);
}

TEST_F(PlayCursorControllerTests, SeekToX_GridSnapOn_SnapsToGrid)
{
    //! CASE With grid snap on, the seek lands on the nearest grid line.
    useGridSnapOn(SnapType::Seconds);

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->seekToX(7.4);

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 7.0);
}

TEST_F(PlayCursorControllerTests, SeekToX_NegativeTime_ClampsToZero_WhenNotAtStart)
{
    //! CASE A negative resulting time seeks to 0, provided we are not already there.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(5.0)));

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->seekToX(-3.0);

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 0.0);
}

TEST_F(PlayCursorControllerTests, SeekToX_NegativeTime_AlreadyAtStart_DoesNotDispatch)
{
    //! CASE Negative time while already at 0 is a no-op.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(0.0)));

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->seekToX(-3.0);
}

TEST_F(PlayCursorControllerTests, SetPlaybackRegion_SnapsAndClampsEdges)
{
    //! CASE Region edges snap to boundaries and are clamped to >= 0.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->setPlaybackRegion(-3.0, 10.4); // start clamps to 0, end snaps to 10.0

    EXPECT_DOUBLE_EQ(captured.param("start").toDouble(), 0.0);
    EXPECT_DOUBLE_EQ(captured.param("end").toDouble(), 10.0);
}
}
