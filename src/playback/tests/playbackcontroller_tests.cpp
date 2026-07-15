/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "audio/tests/mocks/audiodevicesprovidermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "mocks/playbackmock.h"
#include "mocks/playermock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "record/tests/mocks/recordcontrollermock.h"
#include "record/tests/mocks/recordmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

#include "../internal/playbackcontroller.h"

using ::testing::_;
using ::testing::NiceMock;
using ::testing::Return;
using ::testing::ReturnRef;

using namespace muse;
using namespace au;
using namespace au::playback;
using namespace au::context;

static const actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");
static const actions::ActionQuery PLAYBACK_CHANGE_PLAY_REGION_QUERY("action://playback/play-region-change");

namespace au::playback {
class PlaybackControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = new PlaybackController(muse::modularity::globalCtx());

        m_application = std::make_shared<muse::ApplicationMock>();
        m_controller->application.set(m_application);

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        m_controller->globalContext.set(m_globalContext);

        m_dispatcher = std::make_shared<actions::ActionsDispatcherMock>();
        m_controller->dispatcher.set(m_dispatcher);

        m_recordController = std::make_shared<record::RecordControllerMock>();
        m_controller->recordController.set(m_recordController);

        m_record = std::make_shared<NiceMock<record::RecordMock> >();
        m_controller->record.set(m_record);

        m_audioDevicesProvider = std::make_shared<NiceMock<audio::AudioDevicesProviderMock> >();
        m_controller->audioDevicesProvider.set(m_audioDevicesProvider);

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
        m_controller->togglePlayAction();
    }

    void changePlaybackRegion(const secs_t start, const secs_t end)
    {
        muse::actions::ActionQuery q(PLAYBACK_CHANGE_PLAY_REGION_QUERY);
        q.addParam("start", muse::Val(start));
        q.addParam("end", muse::Val(end));
        m_controller->onChangePlaybackRegionAction(q);
    }

    void seek(const secs_t seekTime, const bool triggerPlay = false)
    {
        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(seekTime));
        q.addParam("triggerPlay", muse::Val(triggerPlay));
        m_controller->onSeekAction(q);
    }

    void rewindToStart()
    {
        m_controller->rewindToStartAction();
    }

    void rewindToEnd()
    {
        m_controller->rewindToEndAction();
    }

    void pause()
    {
        m_controller->pauseAction();
    }

    //! withStreamRestart() is a private PlaybackController method (see the friend
    //! declaration in playbackcontroller.h); since friendship is not inherited,
    //! the TEST_F bodies cannot reach it directly, so this thin wrapper in the
    //! friend fixture is how they call it.
    void withStreamRestart(const std::function<void()>& action)
    {
        m_controller->withStreamRestart(action);
    }

    //! m_lastPlaybackRegion is a private PlaybackController field; same
    //! friendship caveat as withStreamRestart() above.
    PlaybackRegion lastPlaybackRegion() const
    {
        return m_controller->m_lastPlaybackRegion;
    }

    //! Makes the player mock behave like a small transport state machine and
    //! records the order in which stop/play/pause happen, so device-change
    //! orchestration can be asserted as an ordered sequence of events.
    void setupStatefulPlayer(PlaybackStatus initial)
    {
        m_status = initial;
        m_events.clear();

        ON_CALL(*m_player, playbackStatus())
        .WillByDefault(::testing::Invoke([this]() { return m_status; }));
        ON_CALL(*m_player, stop())
        .WillByDefault(::testing::Invoke([this]() { m_status = PlaybackStatus::Stopped; m_events.push_back("stop"); }));
        ON_CALL(*m_player, play(_))
        .WillByDefault(::testing::Invoke([this](std::optional<secs_t>) {
            m_status = PlaybackStatus::Running;
            m_events.push_back("play");
        }));
        ON_CALL(*m_player, pause())
        .WillByDefault(::testing::Invoke([this]() { m_status = PlaybackStatus::Paused; m_events.push_back("pause"); }));
    }

    PlaybackStatus m_status = PlaybackStatus::Stopped;
    std::vector<std::string> m_events;

    PlaybackController* m_controller = nullptr;

    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<actions::IActionsDispatcher> m_dispatcher;
    std::shared_ptr<record::RecordControllerMock> m_recordController;
    std::shared_ptr<record::RecordMock> m_record;
    std::shared_ptr<audio::AudioDevicesProviderMock> m_audioDevicesProvider;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    std::shared_ptr<PlaybackMock> m_playback;
    std::shared_ptr<PlayerMock> m_player;
};

/**
 * @brief Toggle play when stopped without selection or loop
 * @details User clicked play without any additional params
 *          Project has content, no selection, no loop active
 *          Playback should start from current stopped position without seeking
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenStopped)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Project has content (totalTime = 100.0)
    //! This is set up in SetUp() via m_trackeditProject->totalTime()

    //! [GIVEN] Playback position is at some position (not at the end)
    secs_t currentPosition = 42.0;
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(currentPosition));

    //! [GIVEN] No time selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(true));

    //! [GIVEN] No loop region active
    EXPECT_CALL(*m_player, isLoopRegionActive())
    .WillRepeatedly(Return(false));

    //! [THEN] No seek should occur (play from current stopped position)
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(0);

    //! [THEN] Playback region falls back to {lastPlaybackSeekTime, totalPlayTime}.
    //! lastPlaybackSeekTime is 0 (default, no prior seek) and totalPlayTime is 100.
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { secs_t(0.0), secs_t(100.0) }))
    .Times(1);

    //! [THEN] Player should start playing from current position
    EXPECT_CALL(*m_player, play(_))
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
    EXPECT_CALL(*m_player, play(_))
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
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(false));
    EXPECT_CALL(*m_selectionController, dataSelectedStartTime())
    .WillOnce(Return(selectionRegion.start));
    EXPECT_CALL(*m_selectionController, dataSelectedEndTime())
    .WillOnce(Return(selectionRegion.end));

    //! [THEN] Expect that we will take into account the selection region
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play when there is a clip selection
 * @details User selected a clip and clicked play
 *          Clip selection does not affect playback: it should run
 *          from the seek position to the end of the project
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithSelection_Clip)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Playback position is at the beginning
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(secs_t(0.0)));

    //! [GIVEN] There is single clip selection from 10 to 20 secs
    ON_CALL(*m_selectionController, leftMostSelectedItemStartTime())
    .WillByDefault(Return(std::optional<secs_t>(secs_t(10.0))));
    ON_CALL(*m_selectionController, rightMostSelectedItemEndTime())
    .WillByDefault(Return(std::optional<secs_t>(secs_t(20.0))));

    //! [GIVEN] No time selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(true));

    //! [THEN] The clip selection is ignored: playback region falls back to
    //! {lastPlaybackSeekTime, totalPlayTime}
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { secs_t(0.0), secs_t(100.0) }))
    .Times(1);

    //! [THEN] No explicit seek (playback runs from the seek position)
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(0);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Toggle play with ignore selection
 * @details User clicked play with Shift modifier
 *          Playback should be started from previous seek position
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
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

    //! [THEN] Expect that playback region will be reseted and playback will be seek to previous seek position
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion()))
    .Times(1);
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
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

TEST_F(PlaybackControllerTests, Pause_WhenSeekTargetChangedDuringPlayback_StopsPlayback)
{
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion()))
    .Times(1);
    EXPECT_CALL(*m_player, stop())
    .Times(1);
    EXPECT_CALL(*m_player, seek(secs_t(12.0), false))
    .Times(1);
    EXPECT_CALL(*m_player, pause())
    .Times(0);

    m_controller->setLastPlaybackSeekTime(12.0);
    pause();
}

TEST_F(PlaybackControllerTests, Pause_WhenPlaybackRegionChangesAfterSeekTargetChange_StillStopsPlayback)
{
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { 3.0, 7.0 }))
    .Times(1);
    EXPECT_CALL(*m_player, stop())
    .Times(1);
    EXPECT_CALL(*m_player, seek(secs_t(3.0), false))
    .Times(1);
    EXPECT_CALL(*m_player, pause())
    .Times(0);

    m_controller->setLastPlaybackSeekTime(12.0);
    changePlaybackRegion(3.0, 7.0);
    pause();
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
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
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

    //! [GIVEN] User started playback
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
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(false));
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
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Fitst: user changed selection
    changePlaybackRegion(selectionRegion.start, selectionRegion.end);

    //! [WHEN] Second: toggle play
    togglePlay();
}

/**
 * @brief Toggle play when there is selection wich start time is more than total time
 * @details User made a selection and clicked play
 *          Playback shouldn't be started
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithSelection_StartTimeIsMoreThanTotalTime)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] There is selection from 10 to 20 secs
    PlaybackRegion selectionRegion = { secs_t(1000.0), secs_t(2000.0) };
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(false));
    EXPECT_CALL(*m_selectionController, dataSelectedStartTime())
    .WillOnce(Return(selectionRegion.start));
    EXPECT_CALL(*m_selectionController, dataSelectedEndTime())
    .WillOnce(Return(selectionRegion.end));

    //! [THEN] Expect that we will take into account the selection region
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Toggle play
    togglePlay();
}

/**
 * @brief Seek playback position to a new time
 * @details User clicked on the clips view
 *          Player should only seek to new time
 */
TEST_F(PlaybackControllerTests, Seek_WhenNotPlaying)
{
    //! [GIVEN] New seek time
    secs_t newSeekTime = 10.0;

    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] Playback will be seek to the new seek position
    EXPECT_CALL(*m_player, seek(newSeekTime, false /* applyIfPlaying */))
    .Times(1);

    //! [WHEN] Seek to the new time
    seek(newSeekTime);
}

/**
 * @brief Seek playback position to a new time when paused
 * @details User clicked on the clips view
 *          Player should stop and seek to new time
 */
TEST_F(PlaybackControllerTests, Seek_WhenPaused)
{
    //! [GIVEN] New seek time
    secs_t newSeekTime = 10.0;

    //! [GIVEN] Playback is paused
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] Playback will be seek to the new seek position
    EXPECT_CALL(*m_player, seek(newSeekTime, false /* applyIfPlaying */))
    .Times(1);

    //! [THEN] Player should stop playing
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [WHEN] Seek to the new time
    seek(newSeekTime);
}

/**
 * @brief Seek playback position to a new time with triggering play
 * @details User clicked on the bottom section of timeline
 *          Player should seek to new time and start playing
 */
TEST_F(PlaybackControllerTests, Seek_WithTriggeringPlay)
{
    //! [GIVEN] New seek time
    secs_t newSeekTime = 10.0;

    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] Playback will be seek to the new seek position
    EXPECT_CALL(*m_player, seek(newSeekTime, true /* applyIfPlaying */))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Seek to the new time with triggering play
    seek(newSeekTime, true);
}

/**
 * @brief Seek playback position to a new time with triggering play and playback is already playing
 * @details User clicked on the bottom section of timeline
 *          Player should only seek to new time
 */
TEST_F(PlaybackControllerTests, Seek_WithTriggeringPlay_AlreadyPlaying)
{
    //! [GIVEN] New seek time
    secs_t newSeekTime = 10.0;

    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [THEN] Playback will be seek to the new seek position
    EXPECT_CALL(*m_player, seek(newSeekTime, true /* applyIfPlaying */))
    .Times(1);

    //! [THEN] Player shouldn't start playing again
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Seek to the new time with triggering play
    seek(newSeekTime, true);
}

/**
 * @brief Seek playback position to a new time that is more than total time with triggering play
 * @details User clicked on the bottom section of timeline
 *          Player should only seek to new time without play
 */
TEST_F(PlaybackControllerTests, Seek_WithTriggeringPlay_FromTimeThatIsMoreThanTotalTime)
{
    //! [GIVEN] New seek time more than total time
    secs_t newSeekTime = 1000.0;

    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] Playback will be seek to the new seek position
    EXPECT_CALL(*m_player, seek(newSeekTime, true /* applyIfPlaying */))
    .Times(1);

    //! [THEN] Player shouldn't start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Seek to the new time with triggering play
    seek(newSeekTime, true);
}

/**
 * @brief Rewind to start
 * @details User clicked rewind to start button
 *         Selection should be cleared
 */
TEST_F(PlaybackControllerTests, Rewind_ToStart_CheckSelectionReset)
{
    //! [GIVEN] No matter of current clip/range selection

    //! [THEN]
    //! Time (clip or range) selection is reset
    EXPECT_CALL(*m_selectionController, resetTimeSelection())
    .Times(1);

    //! [WHEN] Rewind to start
    rewindToStart();
}

/**
 * @brief Rewind to end
 * @details User clicked rewind to end button
 *          Selection should be cleared
 */
TEST_F(PlaybackControllerTests, Rewind_ToEnd_CheckSelectionReset)
{
    //! [GIVEN] No matter of current clip/range selection

    //! [THEN]
    //! Time (clip or range) selection is reset
    EXPECT_CALL(*m_selectionController, resetTimeSelection())
    .Times(1);

    //! [WHEN] Rewind to end
    rewindToEnd();
}

/**
 * @brief Seek then stopSeekAndUpdatePlaybackRegion should keep the cursor.
 * @details User clicks the cursor at 42s, then triggers a stop-and-update
 *          (e.g. via Shift+Space while playing). The playback region forwarded
 *          to the player should be the cursor, not an empty region.
 */
TEST_F(PlaybackControllerTests, StopSeekAndUpdatePlaybackRegion_PreservesSeekPosition)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] No active playback region (seek-validity check uses totalPlayTime)
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion {}));

    const secs_t cursor = 42.0;

    //! [THEN] Player is seeked to the cursor (once by the click, once by
    //! the subsequent stop-and-update)
    EXPECT_CALL(*m_player, seek(cursor, false))
    .Times(2);

    //! [THEN] stopSeekAndUpdatePlaybackRegion stops the player
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [THEN] The playback region forwarded to the player is the cursor
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { cursor, cursor }))
    .Times(1);

    //! [WHEN] User clicks the cursor at 42s
    seek(cursor, false);

    //! [WHEN] Then triggers a stop-and-update
    m_controller->stopSeekAndUpdatePlaybackRegion();
}

/**
 * @brief Toggle play with no selection plays from the cursor to project end.
 * @details Cursor is at 30s (e.g. just after recording finished), nothing
 *          is selected. Pressing Space should set the playback region to
 *          {cursor, totalPlayTime} and start playing.
 */
TEST_F(PlaybackControllerTests, TogglePlay_AfterRecord_PlaysFromSeekToProjectEnd)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Cursor is at 30s
    const secs_t recordEnd = 30.0;
    m_controller->setLastPlaybackSeekTime(recordEnd);

    //! [GIVEN] Playhead is at the cursor (not at project end)
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(recordEnd));

    //! [GIVEN] No selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillOnce(Return(true));

    //! [THEN] Playback region is {cursor, totalPlayTime}
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { recordEnd, secs_t(100.0) }))
    .Times(1);

    //! [THEN] Player starts playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] User presses Space
    togglePlay();
}

/**
 * @brief Changing the device while playing stops, applies the change, then resumes.
 * @details The device can only be switched while no stream is open (issue #11098).
 *          withStreamRestart() must own that: stop playback, apply the change,
 *          and resume playing from the same position — in that order.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePlaying_StopsAppliesResumes)
{
    //! [GIVEN] Playback is running at 30s
    setupStatefulPlayer(PlaybackStatus::Running);
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [THEN] The stream is stopped, the change is applied, then playback resumes;
    //! the playing state is not turned into a pause.
    EXPECT_CALL(*m_player, stop()).Times(1);
    EXPECT_CALL(*m_player, play(_)).Times(1);
    EXPECT_CALL(*m_player, pause()).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([this]() { m_events.push_back("apply"); });

    //! [THEN] The change happened after the stop and before the resume
    EXPECT_EQ(m_events, (std::vector<std::string> { "stop", "apply", "play" }));
}

/**
 * @brief Changing the device while paused stops and stays stopped.
 * @details The stream must be torn down for the switch; the transport is then
 *          left stopped — it is not resumed and not put back into pause.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePaused_StopsWithoutResume)
{
    //! [GIVEN] Playback is paused at 30s
    setupStatefulPlayer(PlaybackStatus::Paused);
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [THEN] The stream is stopped, the change is applied, and nothing resumes
    EXPECT_CALL(*m_player, stop()).Times(1);
    EXPECT_CALL(*m_player, play(_)).Times(0);
    EXPECT_CALL(*m_player, pause()).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([this]() { m_events.push_back("apply"); });

    //! [THEN] Only the stop and the change happened; transport stays stopped
    EXPECT_EQ(m_events, (std::vector<std::string> { "stop", "apply" }));
}

/**
 * @brief Changing the device while stopped only applies the change.
 * @details Nothing is playing, so the transport must not be touched.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhileStopped_OnlyApplies)
{
    //! [GIVEN] Playback is stopped
    setupStatefulPlayer(PlaybackStatus::Stopped);

    //! [THEN] The transport is left alone
    EXPECT_CALL(*m_player, stop()).Times(0);
    EXPECT_CALL(*m_player, play(_)).Times(0);
    EXPECT_CALL(*m_player, pause()).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([this]() { m_events.push_back("apply"); });

    //! [THEN] Only the change itself happened
    EXPECT_EQ(m_events, (std::vector<std::string> { "apply" }));
}

/**
 * @brief Changing the device while recording stops but does not auto-resume.
 * @details A capture stream cannot be resumed from a position, so recording is
 *          stopped (so the low-level switch never sees an open stream) but not
 *          restarted.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhileRecording_StopsWithoutResume)
{
    //! [GIVEN] A recording is in progress
    EXPECT_CALL(*m_recordController, isRecording())
    .WillRepeatedly(Return(true));

    //! [THEN] The recording is stopped but not resumed
    EXPECT_CALL(*m_record, stop()).Times(1);
    EXPECT_CALL(*m_record, start()).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([]() {});
}

/**
 * @brief Resuming after a device change lands back at the interrupted position.
 * @details The stream is torn down and reopened by the switch; the interrupted
 *          position is passed straight to the new stream as its start position.
 *          Nothing else — no seek, no play-region write — so the session and
 *          view state survive the switch untouched.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePlaying_ResumesAtSamePosition)
{
    //! [GIVEN] Playback is running at 30s
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [THEN] The stream is stopped, then restarted at 30s...
    ::testing::InSequence seq;
    EXPECT_CALL(*m_player, stop()).Times(1);
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0)))).Times(1);

    //! [THEN] ...with no other state churn on the player
    EXPECT_CALL(*m_player, seek(_, _)).Times(0);
    EXPECT_CALL(*m_player, setPlaybackRegion(_)).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([]() {});
}

/**
 * @brief Stopping after a device-change resume returns to the original anchor.
 * @details The resume restarts the stream at the interrupted position (see
 *          ChangeAudioDevice_WhilePlaying_ResumesAtSamePosition), but that must
 *          not overwrite lastPlaybackSeekTime — the position a later Stop returns
 *          to should still be where the user last explicitly seeked, not where
 *          the device action happened to catch playback.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePlaying_StopAfterwardsReturnsToOriginalPositionNotResumePosition)
{
    //! [GIVEN] Playback was explicitly seeked to 5s
    m_controller->setLastPlaybackSeekTime(5.0);

    //! [GIVEN] It has since been running, and has advanced to 30s
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [GIVEN] The device is changed; playback resumes at 30s
    withStreamRestart([]() {});

    //! [THEN] Stopping afterwards seeks back to the original anchor (5s), not the
    //! position where the device action took place (30s)
    EXPECT_CALL(*m_player, seek(secs_t(5.0), false))
    .Times(1);

    //! [WHEN] User presses Stop
    m_controller->stopSeekAndUpdatePlaybackRegion();
}

/**
 * @brief Playing again after a device change while paused resumes at the paused position.
 * @details The paused stream can't survive the switch (see
 *          ChangeAudioDevice_WhilePaused_StopsWithoutResume), but the position it
 *          was paused at must not be lost: the next Play should resume there, not
 *          from 0 or from the last explicit seek target.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePaused_TogglePlayAfterwardsResumesFromPausedPosition)
{
    //! [GIVEN] Playback is paused at 30s
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [WHEN] The device is changed; the stream is torn down and left stopped
    withStreamRestart([]() {});

    //! [GIVEN] The transport is now stopped (the paused stream could not survive
    //! the switch)
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] Pressing Play starts the stream at the paused position, without
    //! disturbing the play region or the seek anchor
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0))))
    .Times(1);
    EXPECT_CALL(*m_player, seek(_, _))
    .Times(0);

    //! [WHEN] User presses Play
    togglePlay();
}

TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePlayingWithinSelection_PreservesSelectionEnd)
{
    //! [GIVEN] Playback is stopped with a bounded selection region [10, 20]
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    changePlaybackRegion(10.0, 20.0);

    //! [GIVEN] Playback is now running within that region, currently at 15s
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(15.0)));

    //! [THEN] The stream restarts at 15s; the play region is not touched, so
    //! the selection bounds [10, 20] keep governing where playback stops
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(15.0)))).Times(1);
    EXPECT_CALL(*m_player, seek(_, _)).Times(0);
    EXPECT_CALL(*m_player, setPlaybackRegion(_)).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([]() {});

    //! [THEN] The session's region is the untouched original, not one collapsed
    //! or rewritten around the resume position
    EXPECT_EQ(lastPlaybackRegion(), (PlaybackRegion { secs_t(10.0), secs_t(20.0) }));
}

/**
 * @brief Loop playback resumes at the interrupted position, not the loop start.
 * @details Formerly the resume position was smuggled in by mutating the play
 *          region (seek + setPlaybackRegion), and both of those writes are
 *          no-ops while a loop region is active — so loop playback audibly
 *          restarted at the loop start. Passing the position to play() directly
 *          reaches the stream regardless of loop state (au3's pStartTime, which
 *          the playback policy honors inside an active loop).
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhileLoopPlaying_ResumesAtSamePosition)
{
    //! [GIVEN] A loop region is active and playback is running inside it, at 17s
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(17.0)));

    //! [THEN] The stream restarts at 17s, with no play-region writes for an
    //! active loop to ignore
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(17.0)))).Times(1);
    EXPECT_CALL(*m_player, seek(_, _)).Times(0);
    EXPECT_CALL(*m_player, setPlaybackRegion(_)).Times(0);

    //! [WHEN] The device is changed
    withStreamRestart([]() {});
}

/**
 * @brief A fresh selection made after a device-change pause is honored, not the
 *        stale pre-device-change pause position.
 */
TEST_F(PlaybackControllerTests, ChangeAudioDevice_WhilePaused_ThenFreshSelection_PlaysFreshSelectionNotStalePosition)
{
    //! [GIVEN] Playback is paused at 30s
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(30.0)));

    //! [WHEN] The device is changed; the paused stream can't survive, so 30s is
    //! remembered as the resume position for the next Play
    withStreamRestart([]() {});

    //! [GIVEN] The transport is now stopped, and the user drags a fresh 5-10s
    //! selection before pressing Play again
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    changePlaybackRegion(5.0, 10.0);

    //! [THEN] Pressing Play must not resume at the stale pre-device-change position
    EXPECT_CALL(*m_player, play(_)).Times(::testing::AnyNumber());
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0)))).Times(0);
    EXPECT_CALL(*m_player, seek(secs_t(30.0), _)).Times(0);

    //! [WHEN] User presses Play
    togglePlay();
}
}
