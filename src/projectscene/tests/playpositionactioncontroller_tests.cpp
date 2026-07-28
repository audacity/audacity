/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "projectscene/view/playcursor/playpositionactioncontroller.h"
#include "projectscene/view/timeline/timelinecontext.h"
#include "projectscene/internal/projectviewstate.h"

#include "snaptestaccess.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/audiooutputmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "actions/tests/mocks/actionsdispatchermock.h"

using namespace ::testing;

namespace au::projectscene {
//! Exercises the "move play cursor to selection start/end" actions.
//! These move the play position only — unlike the stepping actions, they must
//! leave the time selection untouched.
class PlayPositionActionControllerTests : public ::testing::Test
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
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
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

        m_context = new TimelineContext();
        SnapTestAccess::wireContext(m_context, m_globalContext, m_playback);

        m_controller = new PlayPositionActionController();
        SnapTestAccess::wirePlayPosition(m_controller, m_globalContext, m_dispatcher, m_selectionController);
        m_controller->setTimelineContext(m_context);
    }

    void TearDown() override
    {
        delete m_controller;
        delete m_context;
    }

    void useTimeSelection(double start, double end)
    {
        ON_CALL(*m_selectionController, timeSelectionIsEmpty())
        .WillByDefault(Return(false));
        ON_CALL(*m_selectionController, dataSelectedStartTime())
        .WillByDefault(Return(muse::secs_t(start)));
        ON_CALL(*m_selectionController, dataSelectedEndTime())
        .WillByDefault(Return(muse::secs_t(end)));
    }

    void useEmptyTimeSelection()
    {
        ON_CALL(*m_selectionController, timeSelectionIsEmpty())
        .WillByDefault(Return(true));
    }

    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;
    std::shared_ptr<ProjectViewState> m_viewState;
    std::shared_ptr<NiceMock<playback::PlaybackMock> > m_playback;
    std::shared_ptr<NiceMock<playback::AudioOutputMock> > m_audioOutput;
    std::shared_ptr<NiceMock<context::PlaybackStateMock> > m_playbackState;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > m_dispatcher;

    TimelineContext* m_context = nullptr;
    PlayPositionActionController* m_controller = nullptr;
};

TEST_F(PlayPositionActionControllerTests, CursorToSelectionStart_SeeksToSelectionStart)
{
    //! CASE The play position moves to the start edge of the time selection.
    useTimeSelection(5.0, 12.0);

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->cursorToSelectionStart();

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 5.0);
    EXPECT_FALSE(captured.param("triggerPlay").toBool());
}

TEST_F(PlayPositionActionControllerTests, CursorToSelectionEnd_SeeksToSelectionEnd)
{
    //! CASE The play position moves to the end edge of the time selection.
    useTimeSelection(5.0, 12.0);

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->cursorToSelectionEnd();

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 12.0);
    EXPECT_FALSE(captured.param("triggerPlay").toBool());
}

TEST_F(PlayPositionActionControllerTests, CursorToSelectionStart_KeepsSelectionIntact)
{
    //! CASE Moving the cursor to an edge must not collapse or alter the
    //! selection — that is the whole difference from the stepping actions.
    useTimeSelection(5.0, 12.0);

    EXPECT_CALL(*m_selectionController, setDataSelectedStartTime(_, _))
    .Times(0);
    EXPECT_CALL(*m_selectionController, setDataSelectedEndTime(_, _))
    .Times(0);

    m_controller->cursorToSelectionStart();
    m_controller->cursorToSelectionEnd();
}

TEST_F(PlayPositionActionControllerTests, CursorToSelection_NoSelection_DoesNothing)
{
    //! CASE With no time selection there is no edge to move to.
    useEmptyTimeSelection();

    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .Times(0);

    m_controller->cursorToSelectionStart();
    m_controller->cursorToSelectionEnd();
}

TEST_F(PlayPositionActionControllerTests, CursorToSelection_WhilePlaying_StillMovesPlayPosition)
{
    //! CASE Unlike a click in the track area, this is an explicit request to
    //! move the play position, so it applies during playback too.
    useTimeSelection(5.0, 12.0);
    ON_CALL(*m_playbackState, isPlaying())
    .WillByDefault(Return(true));

    muse::actions::ActionQuery captured;
    EXPECT_CALL(*m_dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(SaveArg<0>(&captured));

    m_controller->cursorToSelectionStart();

    EXPECT_DOUBLE_EQ(captured.param("seekTime").toDouble(), 5.0);
}
}
