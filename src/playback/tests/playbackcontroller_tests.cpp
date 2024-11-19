/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "actions/tests/mocks/actionsdispatchermock.h"
#include "record/tests/mocks/recordcontrollermock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/playermock.h"
#include "mocks/playbackmock.h"

#include "../internal/playbackcontroller.h"

using ::testing::_;
using ::testing::Return;
using ::testing::ReturnRef;

using namespace muse;
using namespace au;
using namespace au::playback;
using namespace au::context;

namespace au::playback {
class PlaybackControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = new PlaybackController();

        m_application = std::make_shared<muse::ApplicationMock>();
        m_controller->application.set(m_application);

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        m_controller->globalContext.set(m_globalContext);

        m_dispatcher = std::make_shared<actions::ActionsDispatcherMock>();
        m_controller->dispatcher.set(m_dispatcher);

        m_recordController = std::make_shared<record::RecordControllerMock>();
        m_controller->recordController.set(m_recordController);

        m_selectionController = std::make_shared<trackedit::SelectionControllerMock>();
        m_controller->selectionController.set(m_selectionController);

        m_trackeditProject = std::make_shared<trackedit::TrackeditProjectMock>();

        m_currentProject = std::make_shared<project::AudacityProjectMock>();

        m_playback = std::make_shared<PlaybackMock>();
        m_controller->playback.set(m_playback);

        m_player = std::make_shared<PlayerMock>();

        EXPECT_CALL(*m_playback, player(_))
        .WillOnce(Return(m_player));

        EXPECT_CALL(*m_globalContext, setPlayer(_))
        .Times(1);

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        ON_CALL(*m_currentProject, trackeditProject())
        .WillByDefault(Return(m_trackeditProject));

        ON_CALL(*m_trackeditProject, totalTime())
        .WillByDefault(Return(100));

        EXPECT_CALL(*m_recordController, isRecording())
        .WillRepeatedly(Return(false));

        m_controller->init();
    }

    void TearDown() override
    {
        delete m_controller;
    }

    void togglePlay()
    {
        m_controller->togglePlay();
    }

    void changePlaybackRegion(const secs_t start, const secs_t end)
    {
        m_controller->onChangePlaybackRegionAction(muse::actions::ActionData::make_arg2<double, double>(start, end));
    }

    PlaybackController* m_controller = nullptr;

    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<actions::IActionsDispatcher> m_dispatcher;
    std::shared_ptr<record::RecordControllerMock> m_recordController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    std::shared_ptr<PlaybackMock> m_playback;
    std::shared_ptr<PlayerMock> m_player;
};

/**
 * @brief Toggle play when stopped
 * @details User clicked play without any additional params
 *          Playback should be started without seek
 */
TEST_F(PlaybackControllerTests, TogglePlay)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] No seek position
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(0);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when stopped on the end of project
 * @details User clicked play after the previous playback reached the end of project
 *          Playback should be started from start of project (0.0 time)
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenStopped_OnTheEndOfProject)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Was stoped on the end of project
    EXPECT_CALL(*m_player, playbackPosition())
    .WillOnce(Return(secs_t(100.0)));

    //! [THEN] Seek position to start
    EXPECT_CALL(*m_player, seek(secs_t(0.0), false))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when there is selection
 * @details User made a selection and clicked play
 *          Playback should be started from selection's start
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithSelection)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] There is selection from 10 to 20 secs
    PlaybackRegion selectionRegion = { secs_t(10.0), secs_t(20.0) };
    EXPECT_CALL(*m_selectionController, timeSelectionIsNotEmpty())
    .WillOnce(Return(true));
    EXPECT_CALL(*m_selectionController, dataSelectedStartTime())
    .WillOnce(Return(selectionRegion.start));
    EXPECT_CALL(*m_selectionController, dataSelectedEndTime())
    .WillOnce(Return(selectionRegion.end));

    //! [THEN] Expect that we will take into account the selection region
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when there is clips data selection
 * @details User made a selection by double clicking on waveform and clicked play
 *          Playback should be started without seek
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithSelection_Clip)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] There is clip's selection from 10 to 20 secs
    PlaybackRegion selectionRegion = { secs_t(10.0), secs_t(20.0) };
    EXPECT_CALL(*m_selectionController, timeSelectionIsNotEmpty())
    .WillOnce(Return(false));
    EXPECT_CALL(*m_selectionController, selectedClip())
    .WillOnce(Return(trackedit::ClipKey { 1, 1 }));
    EXPECT_CALL(*m_selectionController, selectedClipStartTime())
    .WillOnce(Return(selectionRegion.start));
    EXPECT_CALL(*m_selectionController, selectedClipEndTime())
    .WillOnce(Return(selectionRegion.end));

    //! [THEN] Expect that we will take into account the selection region
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play with ignore selection
 * @details User clicked play with Shift modifier
 *          Playback should ignore selection
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithIgnoreSelection)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Play with Shift modifier
    ON_CALL(*m_application, keyboardModifiers())
    .WillByDefault(Return(Qt::ShiftModifier));

    //! [THEN] No checking selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsNotEmpty())
    .Times(0);

    //! [THEN] Expect that playback region will be reseted and playback will be seek to previous seek position
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion()))
    .Times(1);
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when already playing
 * @details User clicked play again for pause
 *          Playback should be paused
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPlaying)
{
    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [THEN] Player should pause playing
    EXPECT_CALL(*m_player, pause())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when already playing with run from start position
 * @details User clicked play with Shift modifier
 *          Playback should run from start position
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPlaying_PlayAgain)
{
    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [GIVEN] Play with Shift modifier
    ON_CALL(*m_application, keyboardModifiers())
    .WillByDefault(Return(Qt::ShiftModifier));

    //! [THEN] Player should stop playing
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when paused
 * @details User clicked play again for resume
 *          Playback should resume
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPaused)
{
    //! [GIVEN] Playback is paused
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] Player should resume playing
    EXPECT_CALL(*m_player, resume())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when paused with skiping selection
 * @details User clicked play with Shift modifier
 *          Playback should resume from current position with ignoring selection
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPaused_WithIgnoreSelection)
{
    //! [GIVEN] Playback is paused
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [GIVEN] Play with Shift modifier
    ON_CALL(*m_application, keyboardModifiers())
    .WillByDefault(Return(Qt::ShiftModifier));

    //! [THEN] Expect that playbeck should run from current position
    secs_t currentPosition = 10.0;
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(currentPosition));
    EXPECT_CALL(*m_player, seek(currentPosition, false))
    .WillRepeatedly(Return());

    //! [THEN] No checking selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsNotEmpty())
    .Times(0);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when paused with changing selection
 * @details User clicked play after changing selection region
 *          Playback should run from selection start position
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPaused_WithChangingSelection)
{
    //! [GIVEN] Play with Shift modifier
    EXPECT_CALL(*m_application, keyboardModifiers())
    .WillOnce(Return(Qt::ShiftModifier))
    .WillRepeatedly(Return(Qt::NoModifier));

    //! [GIVEN] User started playback and paused it
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    togglePlay();

    //! [GIVEN] And paused it
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    togglePlay();

    //! [GIVEN] In paused state
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] Expect that playbeck should run from selection start position
    PlaybackRegion selectionRegion = { secs_t(10.0), secs_t(20.0) };
    EXPECT_CALL(*m_selectionController, timeSelectionIsNotEmpty())
    .WillOnce(Return(true));
    EXPECT_CALL(*m_selectionController, dataSelectedStartTime())
    .WillOnce(Return(selectionRegion.start));
    EXPECT_CALL(*m_selectionController, dataSelectedEndTime())
    .WillOnce(Return(selectionRegion.end));

    //! [THEN] Expect that we will take into account the selection region
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .WillRepeatedly(Return());

    //! [THEN] Player should stop playing
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play())
    .Times(1);

    //! [GIVEN] Fitst: user changed selection
    changePlaybackRegion(selectionRegion.start, selectionRegion.end);

    //! [WHEN] Second: toggle play
    togglePlay();
}
}
