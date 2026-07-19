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
#include "playback/tests/mocks/playbackcontrollermock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "actions/tests/mocks/actionsdispatchermock.h"

using namespace ::testing;

namespace au::projectscene {
//! Exercises PlayCursorController's seek/play-region dispatch.
//! Snap is applied by the caller via TimelineContext::positionToTime(x, true)
//! before passing the time to seekToTime/setPlaybackRegionByTime.
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
        m_playbackController = std::make_shared<NiceMock<playback::PlaybackControllerMock> >();
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
        SnapTestAccess::wireCursor(m_controller, m_globalContext, m_dispatcher, m_playbackController);
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
    std::shared_ptr<NiceMock<playback::PlaybackControllerMock> > m_playbackController;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::shared_ptr<NiceMock<context::PlaybackStateMock> > m_playbackState;
    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > m_dispatcher;

    TimelineContext* m_context = nullptr;
    PlayCursorController* m_controller = nullptr;
};

TEST_F(PlayCursorControllerTests, SeekToTime_GridSnapOff_SnapsToBoundary)
{
    //! CASE Seeking near an item boundary dispatches a seek at the snapped boundary.
    //! Snap is applied by the caller via positionToTime(x, true).
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    double time = m_context->positionToTime(10.5, true); // within the 4px(=4s at zoom 1) clip-snap tolerance
    m_controller->seekToTime(time);

    EXPECT_TRUE(captured.contains("seekTime"));
    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 10.0);
    EXPECT_FALSE(captured.param("triggerPlay").toBool());
}

TEST_F(PlayCursorControllerTests, SeekToTime_GridSnapOff_FarFromBoundary_UsesRawTime)
{
    //! CASE Outside the snap tolerance, the raw time is used.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    double time = m_context->positionToTime(20.0, true); // 10s away from the only boundary
    m_controller->seekToTime(time);

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 20.0);
}

TEST_F(PlayCursorControllerTests, SeekToTime_GridSnapOn_SnapsToGrid)
{
    //! CASE With grid snap on, the seek lands on the nearest grid line.
    useGridSnapOn(SnapType::Seconds);

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    double time = m_context->positionToTime(7.4, true);
    m_controller->seekToTime(time);

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 7.0);
}

TEST_F(PlayCursorControllerTests, SeekToTime_NegativeTime_ClampsToZero_WhenNotAtStart)
{
    //! CASE A negative time seeks to 0, provided we are not already there.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(5.0)));

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->seekToTime(-3.0);

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 0.0);
}

TEST_F(PlayCursorControllerTests, SeekToTime_NegativeTime_AlreadyAtStart_DoesNotDispatch)
{
    //! CASE Negative time while already at 0 is a no-op.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(0.0)));

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->seekToTime(-3.0);
}

TEST_F(PlayCursorControllerTests, SetPlaybackRegionByTime_SnapsAndClampsEdges)
{
    //! CASE Region edges snap to boundaries and are clamped to >= 0.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    double t1 = m_context->positionToTime(-3.0, true); // negative — clamped to 0 by setPlaybackRegionByTime
    double t2 = m_context->positionToTime(10.4, true); // snaps to 10.0
    m_controller->setPlaybackRegionByTime(t1, t2);

    EXPECT_DOUBLE_EQ(captured.param("start").toDouble(), 0.0);
    EXPECT_DOUBLE_EQ(captured.param("end").toDouble(), 10.0);
}

//! Seek gesture: pressing in the track area must never move the playhead by
//! itself — the seek is deferred to the release, and dropped entirely when the
//! press turned into a drag of something other than the play cursor.
//! Default zoom (1.0) / frame start (0.0) make position and time 1:1.

TEST_F(PlayCursorControllerTests, SeekGesture_Press_DoesNotDispatchSeek)
{
    //! CASE Mousedown alone must not move the playhead.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
}

TEST_F(PlayCursorControllerTests, SeekGesture_PlainClick_SeeksOnRelease)
{
    //! CASE A press + release without dragging seeks to the pressed time on release.
    useGridSnapOff({});

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    EXPECT_TRUE(m_controller->endSeekGesture());

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 20.0);
    EXPECT_FALSE(captured.param("triggerPlay").toBool());
}

TEST_F(PlayCursorControllerTests, SeekGesture_ClickWithinDragThreshold_StillSeeks)
{
    //! CASE Small jitter below the drag threshold is still a click.
    useGridSnapOff({});

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->updateSeekGesture(22.0, 52.0); // < 5px in both axes
    EXPECT_TRUE(m_controller->endSeekGesture());

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 20.0);
}

TEST_F(PlayCursorControllerTests, SeekGesture_ClickAppliesSnap)
{
    //! CASE The deferred seek is snapped exactly like a direct seekToTime.
    useGridSnapOff({ 10.0 });

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->beginSeekGesture(10.5, 10.5, 50.0); // within the 4px(=4s at zoom 1) clip-snap tolerance
    EXPECT_TRUE(m_controller->endSeekGesture());

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 10.0);
}

TEST_F(PlayCursorControllerTests, SeekGesture_HorizontalDrag_DoesNotSeek)
{
    //! CASE Dragging (e.g. a time selection) suppresses the seek: no dispatch
    //! during the drag and none on release.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->updateSeekGesture(30.0, 50.0);
    EXPECT_FALSE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_VerticalDrag_DoesNotSeek)
{
    //! CASE A vertical drag (multi-track selection) is a drag too.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->updateSeekGesture(20.0, 80.0);
    EXPECT_FALSE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_DragReturningToOrigin_StillNoSeek)
{
    //! CASE Once the drag threshold is crossed the gesture stays a drag, even
    //! if the pointer returns to the press position before release.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->updateSeekGesture(40.0, 50.0);
    m_controller->updateSeekGesture(20.0, 50.0);
    EXPECT_FALSE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_Cancel_DoesNotSeek)
{
    //! CASE A canceled gesture (e.g. the press turned into an item drag or a
    //! double-click took over) never seeks.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->cancelSeekGesture();
    EXPECT_FALSE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_EndWithoutBegin_IsNoop)
{
    //! CASE Releases without a matching press (item drags never begin a
    //! gesture) do nothing.
    useGridSnapOff({});

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    EXPECT_FALSE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_ClickWhilePlaying_RecordsSeekTime_DoesNotMovePlayhead)
{
    //! CASE Clicking during playback must not interrupt playback: no seek is
    //! dispatched, but the click position is remembered so that stopping
    //! returns there.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, isPlaying())
    .WillByDefault(Return(true));

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);
    EXPECT_CALL(*m_playbackController, setLastPlaybackSeekTime(muse::secs_t(20.0)));

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    EXPECT_TRUE(m_controller->endSeekGesture());
}

TEST_F(PlayCursorControllerTests, SeekGesture_DragWhilePlaying_DoesNotTouchPlayback)
{
    //! CASE Dragging during playback leaves playback completely alone —
    //! no seek, no seek-time bookkeeping.
    useGridSnapOff({});
    ON_CALL(*m_playbackState, isPlaying())
    .WillByDefault(Return(true));

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);
    EXPECT_CALL(*m_playbackController, setLastPlaybackSeekTime(_))
    .Times(0);

    m_controller->beginSeekGesture(20.0, 20.0, 50.0);
    m_controller->updateSeekGesture(60.0, 50.0);
    EXPECT_FALSE(m_controller->endSeekGesture());
}
}
