/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "au3audio/tests/mocks/audioenginemock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "interactive/tests/mocks/interactivemock.h"
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
using ::testing::Property;
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

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        m_controller->globalContext.set(m_globalContext);

        m_dispatcher = std::make_shared<actions::ActionsDispatcherMock>();
        m_controller->dispatcher.set(m_dispatcher);

        m_interactive = std::make_shared<NiceMock<InteractiveMock> >();
        m_controller->interactive.set(m_interactive);

        m_recordController = std::make_shared<record::RecordControllerMock>();
        m_controller->recordController.set(m_recordController);

        m_record = std::make_shared<NiceMock<record::RecordMock> >();
        m_controller->record.set(m_record);

        m_audioDriverController = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_controller->audioDriverController.set(m_audioDriverController);

        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_controller->audioEngine.set(m_audioEngine);

        m_selectionController = std::make_shared<trackedit::SelectionControllerMock>();
        m_controller->selectionController.set(m_selectionController);

        m_trackeditProject = std::make_shared<trackedit::TrackeditProjectMock>();

        m_currentProject = std::make_shared<project::AudacityProjectMock>();

        m_playback = std::make_shared<PlaybackMock>();
        m_controller->playback.set(m_playback);

        m_player = std::make_shared<PlayerMock>();

        //! NOTE: use a persistent channel so tests can inject playback position events
        ON_CALL(*m_player, playbackPositionChanged())
        .WillByDefault(Return(m_playbackPositionChanged));

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

    //! NOTE: toolbar Play/Pause button — play/pause
    void togglePlayPause()
    {
        m_controller->togglePlayPauseAction();
    }

    //! NOTE: Spacebar — play/stop
    void togglePlayStop()
    {
        m_controller->togglePlayStopAction();
    }

    //! NOTE: Shift+Spacebar — play from cursor, ignoring selection (pause while playing)
    void togglePlayFromCursor()
    {
        m_controller->togglePlayFromCursorAction();
    }

    void playSelection()
    {
        m_controller->playSelectionAction();
    }

    void stop()
    {
        m_controller->stopAction();
    }

    void setRecording(bool isRecording, bool isLeadIn = false)
    {
        EXPECT_CALL(*m_recordController, isRecording())
        .WillRepeatedly(Return(isRecording));

        EXPECT_CALL(*m_recordController, isLeadInRecording())
        .WillRepeatedly(Return(isLeadIn));
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

    audio::AudioStreamRestorer suspend(audio::AudioStreamKind kind)
    {
        return m_controller->suspendForAudioConfiguration(kind);
    }

    void changeAudioApi(int index)
    {
        muse::actions::ActionQuery q("action://playback/change-api");
        q.addParam("api_index", muse::Val(index));
        m_controller->setAudioApi(q);
    }

    void changeInputDevice(int index)
    {
        muse::actions::ActionQuery q("action://playback/change-recording-device");
        q.addParam("device_index", muse::Val(index));
        m_controller->setAudioInputDevice(q);
    }

    void playFromCurrentState()
    {
        m_controller->doPlay(false);
    }

    void rescanAudioDevices()
    {
        m_controller->rescanAudioDevices();
    }

    PlaybackController* m_controller = nullptr;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<InteractiveMock> m_interactive;
    std::shared_ptr<record::RecordControllerMock> m_recordController;
    std::shared_ptr<record::RecordMock> m_record;
    std::shared_ptr<audio::AudioDriverControllerMock> m_audioDriverController;
    std::shared_ptr<audio::AudioEngineMock> m_audioEngine;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    std::shared_ptr<PlaybackMock> m_playback;
    std::shared_ptr<PlayerMock> m_player;

    muse::async::Channel<muse::secs_t> m_playbackPositionChanged;
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

    //! [GIVEN] The selection is never consulted for playback
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

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

    //! [THEN] Player should start playing from the seek anchor (0.0, no prior seek)
    EXPECT_CALL(*m_player, play(std::optional<muse::secs_t>(secs_t(0.0))))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayStop();
}

/**
 * @brief Toggle play passes the playhead position to the player
 * @details With an active loop region the play region cannot be updated, so the
 *          explicit start time is what makes playback start from the playhead
 *          instead of the loop region start (issue #11074)
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenStopped_PassesPlayheadToPlayer)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Playhead is at 5 secs
    const secs_t playheadPosition = 5.0;
    EXPECT_CALL(*m_player, seek(playheadPosition, false))
    .Times(1);
    seek(playheadPosition);

    //! [GIVEN] A loop region is active
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));

    //! [THEN] Player starts playing from the playhead position, not the loop start
    EXPECT_CALL(*m_player, play(std::optional<muse::secs_t>(playheadPosition)))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayStop();
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
    togglePlayStop();
}

/**
 * @brief Toggle play when stopped at the end of a played selection/region
 * @details Playing a selection leaves the playhead at the region end (mid-project).
 *          The next play must continue from the playhead, not jump back to project start.
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenStopped_OnTheEndOfPlaybackRegion)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] The played region was a selection {10, 20} and the playhead is at its end
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion { secs_t(10.0), secs_t(20.0) }));
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(secs_t(20.0)));
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(false));

    //! [THEN] Continue from the playhead (20), not from the project start
    EXPECT_CALL(*m_player, seek(secs_t(20.0), false))
    .Times(1);
    EXPECT_CALL(*m_player, seek(secs_t(0.0), _))
    .Times(0);

    //! [THEN] Playback region runs from the playhead to the project end
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { secs_t(20.0), secs_t(100.0) }))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayStop();
}

/**
 * @brief Toggle play when there is selection
 * @details User made a selection, placed the playhead before it and clicked play
 *          A time selection must not constrain playback: it should start from
 *          the playhead and flow through the selection to the project end
 */
TEST_F(PlaybackControllerTests, TogglePlay_WithSelection_PlaysFromPlayheadThroughSelection)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] Playhead is at 5 secs
    const secs_t playheadPosition = 5.0;
    EXPECT_CALL(*m_player, seek(playheadPosition, false))
    .Times(1);
    seek(playheadPosition);

    //! [GIVEN] There is selection from 10 to 20 secs
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(secs_t(10.0)));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(secs_t(20.0)));

    //! [THEN] The selection is never consulted for playback
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

    //! [THEN] Playback region is {playhead, totalPlayTime}, not the selection
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { playheadPosition, secs_t(100.0) }))
    .Times(1);

    //! [THEN] Player should start playing from the playhead, not the selection start
    EXPECT_CALL(*m_player, play(std::optional<muse::secs_t>(playheadPosition)))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayStop();
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

    //! [GIVEN] The selection is never consulted for playback
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

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
    togglePlayStop();
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
    togglePlayFromCursor();
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
    togglePlayPause();
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

    //! [THEN] Player should stop playing
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayStop();
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
    togglePlayStop();
}

/**
 * @brief Toggle play when paused with an active loop region resumes
 * @details With a loop region active the player's play region (the loop region) never
 *          matches the last requested playback region; that mismatch must not be
 *          mistaken for a playback region change — play after pause must resume from the
 *          paused position, not restart
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPaused_WithActiveLoop_Resumes)
{
    //! [GIVEN] Playback started while stopped, giving a valid last playback region
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    changePlaybackRegion(0.0, 100.0);

    //! [GIVEN] A loop region is active; the player's play region is the loop region
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion { secs_t(10.0), secs_t(20.0) }));

    //! [GIVEN] Playback is paused
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] Playback resumes from the paused position — no stop, no restart
    EXPECT_CALL(*m_player, resume())
    .Times(1);
    EXPECT_CALL(*m_player, stop())
    .Times(0);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Toggle play
    togglePlayStop();
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

    //! [THEN] Expect that playbeck should run from current position
    secs_t currentPosition = 10.0;
    EXPECT_CALL(*m_player, playbackPosition())
    .WillRepeatedly(Return(currentPosition));
    EXPECT_CALL(*m_player, seek(currentPosition, false))
    .WillRepeatedly(Return());

    //! [THEN] No checking selection
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

    //! [THEN] Player should start playing without an explicit start time —
    //! the player asserts that no start time is passed when resuming from pause
    EXPECT_CALL(*m_player, play(std::optional<muse::secs_t>(std::nullopt)))
    .Times(1);

    //! [WHEN] Toggle play
    togglePlayFromCursor();
}

/**
 * @brief Toggle play when paused with changing selection
 * @details User clicked play after changing selection region
 *          Playback should run from selection start position
 */
TEST_F(PlaybackControllerTests, TogglePlay_WhenPaused_WithChangingSelection)
{
    //! [GIVEN] User started playback, then paused it
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    togglePlayStop();

    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    togglePlayPause();

    //! [GIVEN] In paused state
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] Expect that playback should restart from the new selection start
    PlaybackRegion selectionRegion = { secs_t(10.0), secs_t(20.0) };

    //! [THEN] Player should stop, then start playing
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] First: user changed selection
    changePlaybackRegion(selectionRegion.start, selectionRegion.end);

    //! [WHEN] Second: press Space
    togglePlayStop();

    //! [THEN] Playback restarted from the new selection start
    EXPECT_EQ(m_controller->lastPlaybackSeekTime(), selectionRegion.start);
}

/**
 * @brief Toggle play when the playback region start time is more than total time
 * @details The playback region was set past the project end
 *          Playback shouldn't be started
 */
TEST_F(PlaybackControllerTests, TogglePlay_StartTimeIsMoreThanTotalTime)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] The playback region is past the project end (totalTime = 100)
    PlaybackRegion region = { secs_t(1000.0), secs_t(2000.0) };

    //! [THEN] The region is forwarded to the player (once by the region change,
    //! once by the invalid-region fallback in doPlay)
    EXPECT_CALL(*m_player, setPlaybackRegion(region))
    .Times(2);

    //! [THEN] Player shouldn't start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Change the playback region and toggle play
    changePlaybackRegion(region.start, region.end);
    togglePlayStop();
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

    //! [GIVEN] The selection is never consulted for playback
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty())
    .Times(0);

    //! [THEN] Playback region is {cursor, totalPlayTime}
    EXPECT_CALL(*m_player, setPlaybackRegion(PlaybackRegion { recordEnd, secs_t(100.0) }))
    .Times(1);

    //! [THEN] Player starts playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] User presses Space
    togglePlayStop();
}

/**
 * @brief Playback does not stop when passing a former selection end
 * @details Playback runs with region {5, 100} (playhead to project end).
 *          Reaching a position in the middle (e.g. the end of a time selection
 *          at 20s) must not stop the player.
 */
TEST_F(PlaybackControllerTests, PlaybackPosition_InsideRegion_DoesNotStop)
{
    //! [GIVEN] Playback is running with region {5, 100}
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion { secs_t(5.0), secs_t(100.0) }));
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(false));

    //! [GIVEN] Playback position is at 20s (inside the region)
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(20.0)));

    //! [THEN] Player is not stopped
    EXPECT_CALL(*m_player, stop())
    .Times(0);

    //! [WHEN] Playback position changed
    m_playbackPositionChanged.send(secs_t(20.0));
}

/**
 * @brief Playback stops at the end of the playback region / project
 */
TEST_F(PlaybackControllerTests, PlaybackPosition_OnRegionEnd_Stops)
{
    //! [GIVEN] The playback region is {5, 100}
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    changePlaybackRegion(5.0, 100.0);

    //! [GIVEN] Playback is running with that region
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion { secs_t(5.0), secs_t(100.0) }));
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(false));

    //! [GIVEN] Playback position reached the region end
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(100.0)));

    //! [THEN] Player is stopped
    EXPECT_CALL(*m_player, stop())
    .Times(1);

    //! [WHEN] Playback position changed
    m_playbackPositionChanged.send(secs_t(100.0));
}

/**
 * @brief Loop playback is not stopped at the loop region end
 */
TEST_F(PlaybackControllerTests, PlaybackPosition_OnLoopRegionEnd_DoesNotStop)
{
    //! [GIVEN] Playback is running with an active loop region {10, 20}
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    ON_CALL(*m_player, playbackRegion())
    .WillByDefault(Return(PlaybackRegion { secs_t(10.0), secs_t(20.0) }));
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));

    //! [GIVEN] Playback position reached the loop region end
    ON_CALL(*m_player, playbackPosition())
    .WillByDefault(Return(secs_t(20.0)));

    //! [THEN] Player is not stopped (the loop wraps around)
    EXPECT_CALL(*m_player, stop())
    .Times(0);

    //! [WHEN] Playback position changed
    m_playbackPositionChanged.send(secs_t(20.0));
}

/**
 * @brief Play selection plays exactly the selected time range
 * @details User triggered the "play-selection" action with a selection of 10-20 secs
 *          Playback region should be the selection, so playback starts at 10s
 *          and stops at 20s (via the end-of-region check)
 */
TEST_F(PlaybackControllerTests, PlaySelection_WithSelection)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] There is selection from 10 to 20 secs
    PlaybackRegion selectionRegion = { secs_t(10.0), secs_t(20.0) };
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionRegion.start));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionRegion.end));

    //! [THEN] The playback region is the selection
    EXPECT_CALL(*m_player, setPlaybackRegion(selectionRegion))
    .Times(1);

    //! [THEN] Player should start playing
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Play selection
    playSelection();

    //! [THEN] The playback cursor is at the selection start
    EXPECT_EQ(m_controller->lastPlaybackSeekTime(), selectionRegion.start);
}

/**
 * @brief Play selection with an active loop region plays the selection, not the loop
 * @details The play region cannot be updated while a loop region is active, so the
 *          selection must be played as an explicit range (issue #9393)
 */
TEST_F(PlaybackControllerTests, PlaySelection_WithActiveLoop_PlaysSelectionRange)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] A loop region is active
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));

    //! [GIVEN] There is selection from 5 to 7 secs
    PlaybackRegion selectionRegion = { secs_t(5.0), secs_t(7.0) };
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionRegion.start));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionRegion.end));

    //! [THEN] The selection is played as an explicit range, not via the play region
    EXPECT_CALL(*m_player, playRange(selectionRegion))
    .Times(1);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Play selection
    playSelection();
}

/**
 * @brief Play selection while playing with an active loop restarts with the selection range
 * @details Active playback is stopped first, then the selection is played as an
 *          explicit range because the loop region blocks play region updates
 */
TEST_F(PlaybackControllerTests, PlaySelection_WhilePlaying_WithActiveLoop_Restarts)
{
    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [GIVEN] A loop region is active
    ON_CALL(*m_player, isLoopRegionActive())
    .WillByDefault(Return(true));

    //! [GIVEN] There is selection from 5 to 7 secs
    PlaybackRegion selectionRegion = { secs_t(5.0), secs_t(7.0) };
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionRegion.start));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionRegion.end));

    //! [THEN] Player is stopped, then the selection is played as an explicit range
    EXPECT_CALL(*m_player, stop())
    .Times(1);
    EXPECT_CALL(*m_player, playRange(selectionRegion))
    .Times(1);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Play selection
    playSelection();
}

/**
 * @brief Play selection does nothing without a time selection
 */
TEST_F(PlaybackControllerTests, PlaySelection_WithoutSelection_DoesNothing)
{
    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [GIVEN] No time selection
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(true));

    //! [THEN] Nothing happens
    EXPECT_CALL(*m_player, setPlaybackRegion(_))
    .Times(0);
    EXPECT_CALL(*m_player, stop())
    .Times(0);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Play selection
    playSelection();
}

/**
 * @brief Play selection availability depends on selection and playback state
 */
TEST_F(PlaybackControllerTests, PlaySelection_CanReceiveAction)
{
    const muse::actions::ActionCode code = "action://playback/play-selection";

    //! [GIVEN] Playback is stopped
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] Without a selection the action is unavailable
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(true));
    EXPECT_FALSE(m_controller->canReceiveAction(code));

    //! [THEN] With a selection it is available
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    EXPECT_TRUE(m_controller->canReceiveAction(code));

    //! [THEN] While playing it is available even without a selection (it stops playback)
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(true));
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));
    EXPECT_TRUE(m_controller->canReceiveAction(code));
}

/**
 * @brief Play selection without a selection stops active playback
 */
TEST_F(PlaybackControllerTests, PlaySelection_WhilePlaying_NoSelection_Stops)
{
    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [GIVEN] No time selection
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(true));

    //! [THEN] Player is stopped and not restarted
    EXPECT_CALL(*m_player, stop())
    .Times(1);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Play selection
    playSelection();
}

/**
 * @brief Play selection does nothing while recording
 */
TEST_F(PlaybackControllerTests, PlaySelection_WhileRecording_DoesNothing)
{
    //! [GIVEN] Recording is in progress
    EXPECT_CALL(*m_recordController, isRecording())
    .WillRepeatedly(Return(true));

    //! [GIVEN] There is selection from 10 to 20 secs
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(secs_t(10.0)));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(secs_t(20.0)));

    //! [THEN] The action is not available and nothing happens
    EXPECT_FALSE(m_controller->canReceiveAction("action://playback/play-selection"));

    EXPECT_CALL(*m_player, setPlaybackRegion(_))
    .Times(0);
    EXPECT_CALL(*m_player, stop())
    .Times(0);
    EXPECT_CALL(*m_player, play(_))
    .Times(0);

    //! [WHEN] Play selection
    playSelection();
}

/**
 * @brief Play selection while playing restarts playback from the selection
 */
TEST_F(PlaybackControllerTests, PlaySelection_WhilePlaying_Restarts)
{
    //! [GIVEN] Playback is running
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [GIVEN] There is selection from 10 to 20 secs
    ON_CALL(*m_selectionController, timeSelectionIsEmpty())
    .WillByDefault(Return(false));
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(secs_t(10.0)));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(secs_t(20.0)));

    //! [THEN] Player is stopped, then restarted
    EXPECT_CALL(*m_player, stop())
    .Times(1);
    EXPECT_CALL(*m_player, play(_))
    .Times(1);

    //! [WHEN] Play selection
    playSelection();

    //! [THEN] The playback cursor is at the selection start
    EXPECT_EQ(m_controller->lastPlaybackSeekTime(), secs_t(10.0));
}

TEST_F(PlaybackControllerTests, SuspendPlayback_RestoresAtInterruptedPosition)
{
    PlaybackStatus status = PlaybackStatus::Running;
    ON_CALL(*m_player, playbackStatus()).WillByDefault([&status]() { return status; });
    ON_CALL(*m_player, playbackPosition()).WillByDefault(Return(secs_t(30.0)));

    EXPECT_CALL(*m_player, stop()).WillOnce([&status]() { status = PlaybackStatus::Stopped; });
    auto restoreStream = suspend(audio::AudioStreamKind::Playback);

    ASSERT_TRUE(restoreStream);
    EXPECT_EQ(status, PlaybackStatus::Stopped);
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0))))
    .WillOnce([&status](std::optional<secs_t>) { status = PlaybackStatus::Running; });
    EXPECT_TRUE(restoreStream());
    EXPECT_EQ(status, PlaybackStatus::Running);
}

TEST_F(PlaybackControllerTests, SuspendRunningPlayback_TransportStateTakesPrecedenceOverPhysicalKind)
{
    PlaybackStatus status = PlaybackStatus::Running;
    ON_CALL(*m_player, playbackStatus()).WillByDefault([&status]() { return status; });
    ON_CALL(*m_player, playbackPosition()).WillByDefault(Return(secs_t(30.0)));

    EXPECT_CALL(*m_player, stop()).WillOnce([&status]() { status = PlaybackStatus::Stopped; });
    EXPECT_CALL(*m_record, stop()).Times(0);
    auto restoreStream = suspend(audio::AudioStreamKind::Recording);

    ASSERT_TRUE(restoreStream);
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0))))
    .WillOnce([&status](std::optional<secs_t>) { status = PlaybackStatus::Running; });
    EXPECT_TRUE(restoreStream());
    EXPECT_EQ(status, PlaybackStatus::Running);
}

TEST_F(PlaybackControllerTests, SuspendPausedPlayback_NextPlayUsesDurablePausePosition)
{
    PlaybackStatus status = PlaybackStatus::Paused;
    ON_CALL(*m_player, playbackStatus()).WillByDefault([&status]() { return status; });
    ON_CALL(*m_player, playbackPosition()).WillByDefault(Return(secs_t(30.0)));

    EXPECT_CALL(*m_player, stop()).WillOnce([&status]() { status = PlaybackStatus::Stopped; });
    auto restoreStream = suspend(audio::AudioStreamKind::Playback);

    ASSERT_TRUE(restoreStream);
    EXPECT_TRUE(restoreStream());
    //! The resume path no longer consults the selection: any explicit
    //! reposition since the teardown clears the pending position instead.
    EXPECT_CALL(*m_selectionController, timeSelectionIsEmpty()).Times(0);
    EXPECT_CALL(*m_player, play(std::optional<secs_t>(secs_t(30.0)))).Times(1);

    playFromCurrentState();
}

TEST_F(PlaybackControllerTests, SuspendRecording_UsesPhysicalBackstopWhenControllerStateLags)
{
    const audio::AudioStreamDescriptor recordingStream { audio::AudioStreamKind::Recording, nullptr, 44100.0 };
    EXPECT_CALL(*m_recordController, isRecording()).WillRepeatedly(Return(false));
    EXPECT_CALL(*m_record, stop()).WillOnce(Return(muse::make_ok()));
    EXPECT_CALL(*m_audioEngine, currentStream())
    .WillOnce(Return(recordingStream))
    .WillOnce(Return(std::nullopt));
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);
    EXPECT_CALL(*m_record, start()).Times(0);

    auto restoreStream = suspend(audio::AudioStreamKind::Recording);

    ASSERT_TRUE(restoreStream);
    EXPECT_TRUE(restoreStream());
}

TEST_F(PlaybackControllerTests, SuspendRecording_FailsWhenTheStreamCannotBeStopped)
{
    EXPECT_CALL(*m_record, stop()).WillOnce(Return(muse::make_ret(muse::Ret::Code::UnknownError)));
    EXPECT_CALL(*m_audioEngine, currentStream()).Times(0);

    EXPECT_FALSE(suspend(audio::AudioStreamKind::Recording));
}

TEST_F(PlaybackControllerTests, SuspendRecording_FailsClosedWhenPhysicalBackstopDoesNotStopStream)
{
    const audio::AudioStreamDescriptor recordingStream { audio::AudioStreamKind::Recording, nullptr, 44100.0 };
    EXPECT_CALL(*m_record, stop()).WillOnce(Return(muse::make_ok()));
    EXPECT_CALL(*m_audioEngine, currentStream()).WillRepeatedly(Return(recordingStream));
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);

    EXPECT_FALSE(suspend(audio::AudioStreamKind::Recording));
}

TEST_F(PlaybackControllerTests, SuspendPlaybackStopsStreamDuringTeardown)
{
    const audio::AudioStreamDescriptor playbackStream { audio::AudioStreamKind::Playback, nullptr, 44100.0 };
    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));
    EXPECT_CALL(*m_audioEngine, currentStream())
    .WillOnce(Return(playbackStream))
    .WillOnce(Return(std::nullopt));
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);

    EXPECT_NE(suspend(audio::AudioStreamKind::Playback), nullptr);
}

TEST_F(PlaybackControllerTests, SuspendMonitoring_RestoresTheOwningProject)
{
    static int dummyProject;
    ON_CALL(*m_currentProject, au3ProjectPtr())
    .WillByDefault(Return(reinterpret_cast<uintptr_t>(&dummyProject)));
    ON_CALL(*m_audioEngine, isMonitoring()).WillByDefault(Return(true));
    ON_CALL(*m_audioDriverController, inputDevices())
    .WillByDefault(Return(std::vector<std::string> { "Built-in microphone" }));
    ON_CALL(*m_audioDriverController, inputChannelsAvailable())
    .WillByDefault(Return(2));

    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    auto restoreStream = suspend(audio::AudioStreamKind::Monitoring);

    ASSERT_TRUE(restoreStream);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(1);
    EXPECT_TRUE(restoreStream());
}

TEST_F(PlaybackControllerTests, SuspendMonitoring_WithoutRecordDevice_DoesNotRestartMonitoring)
{
    static int dummyProject;
    ON_CALL(*m_currentProject, au3ProjectPtr())
    .WillByDefault(Return(reinterpret_cast<uintptr_t>(&dummyProject)));
    ON_CALL(*m_audioEngine, isMonitoring()).WillByDefault(Return(true));
    ON_CALL(*m_audioDriverController, inputDevices())
    .WillByDefault(Return(std::vector<std::string> {}));

    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    auto restoreStream = suspend(audio::AudioStreamKind::Monitoring);

    ASSERT_TRUE(restoreStream);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(0);
    EXPECT_TRUE(restoreStream());
}

TEST_F(PlaybackControllerTests, ChangeAudioApi_SubmitsACompleteTypedChange)
{
    ON_CALL(*m_audioDriverController, apis())
    .WillByDefault(Return(std::vector<std::string> { "Core Audio", "JACK" }));

    audio::AudioConfigurationChange captured;
    EXPECT_CALL(*m_audioDriverController, apply(muse::modularity::globalCtx(), _))
    .WillOnce([&captured](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        captured = change;
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    changeAudioApi(1);

    ASSERT_TRUE(captured.api.has_value());
    EXPECT_EQ(*captured.api, "JACK");
    EXPECT_FALSE(captured.outputDevice.has_value());
    EXPECT_FALSE(captured.inputDevice.has_value());
}

TEST_F(PlaybackControllerTests, ChangeInputDevice_RejectedChangeRestoresMenuStateAndReportsError)
{
    ON_CALL(*m_audioDriverController, inputDevices())
    .WillByDefault(Return(std::vector<std::string> { "Built-in microphone", "USB microphone" }));

    EXPECT_CALL(*m_audioDriverController, apply(muse::modularity::globalCtx(), _))
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::OwnerUnavailable }));
    EXPECT_CALL(*m_interactive, error(_, _, _, _, _, _))
    .WillOnce(Return(muse::async::make_promise<muse::IInteractive::Result>(
                         [](const auto& resolve) { return resolve(muse::IInteractive::Result {}); },
                         muse::async::PromiseType::AsyncByBody)));

    std::vector<muse::actions::ActionCode> changedActions;
    m_controller->actionCheckedChanged().onReceive(nullptr, [&changedActions](const muse::actions::ActionCode& code) {
        changedActions.push_back(code);
    });

    changeInputDevice(1);

    EXPECT_THAT(changedActions, ::testing::ElementsAre("action://playback/change-recording-device"));
}

TEST_F(PlaybackControllerTests, RescanAudioDevices_DelegatesToGlobalController)
{
    EXPECT_CALL(*m_audioDriverController, rescan())
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::Applied }));

    rescanAudioDevices();
}

TEST_F(PlaybackControllerTests, Stop_WhenRecording_StopsTheRecorder)
{
    //! [GIVEN] Recording is running
    setRecording(true);

    //! [THEN] The recorder is stopped, not the player
    EXPECT_CALL(*m_dispatcher, dispatch(::testing::Matcher<const muse::actions::ActionQuery&>(
                                            Property(&muse::actions::ActionQuery::toString, "action://record/stop"))))
    .Times(1);

    EXPECT_CALL(*m_player, stop())
    .Times(0);

    //! [WHEN] User presses the Stop button
    stop();
}

TEST_F(PlaybackControllerTests, TogglePlayPause_WhenRecording_PausesTheRecorder)
{
    //! [GIVEN] Recording is running (not in lead-in)
    setRecording(true, false /* isLeadIn */);

    //! [THEN] The recorder is paused, not the player
    EXPECT_CALL(*m_dispatcher, dispatch(::testing::Matcher<const muse::actions::ActionQuery&>(
                                            Property(&muse::actions::ActionQuery::toString, "action://record/pause"))))
    .Times(1);

    EXPECT_CALL(*m_player, pause())
    .Times(0);

    //! [WHEN] User presses the Play/Pause button
    togglePlayPause();
}

TEST_F(PlaybackControllerTests, TogglePlayPause_DuringLeadIn_PausesThePlayback)
{
    //! [GIVEN] The record lead-in pre-roll is playing back. The audio is driven by the
    //! record stream, not the player, so the player status is not Running.
    setRecording(true, true /* isLeadIn */);

    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Stopped));

    //! [THEN] The shared stream is paused, the recorder is not touched
    EXPECT_CALL(*m_player, pause())
    .Times(1);

    EXPECT_CALL(*m_dispatcher, dispatch(::testing::Matcher<const muse::actions::ActionQuery&>(
                                            Property(&muse::actions::ActionQuery::toString, "action://record/pause"))))
    .Times(0);

    //! [WHEN] User presses the Play/Pause button during lead-in
    togglePlayPause();
}

TEST_F(PlaybackControllerTests, TogglePlayPause_DuringLeadInWhenPaused_ResumesThePlayback)
{
    //! [GIVEN] The lead-in pre-roll has been paused (player status is Paused, but the
    //! recorder is still in lead-in)
    setRecording(true, true /* isLeadIn */);

    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Paused));

    //! [THEN] The shared stream resumes, the recorder is not touched
    EXPECT_CALL(*m_player, resume())
    .Times(1);

    EXPECT_CALL(*m_dispatcher, dispatch(::testing::Matcher<const muse::actions::ActionQuery&>(
                                            Property(&muse::actions::ActionQuery::toString, "action://record/pause"))))
    .Times(0);

    //! [WHEN] User presses the Play/Pause button to resume the lead-in
    togglePlayPause();
}

TEST_F(PlaybackControllerTests, IsPlaying_WhileRecording_ReportsFalse)
{
    //! [GIVEN] Recording is running while the player is left "running" (e.g. after
    //! resuming a lead-in through the shared stream)
    setRecording(true);

    ON_CALL(*m_player, playbackStatus())
    .WillByDefault(Return(PlaybackStatus::Running));

    //! [THEN] The controller reports not-playing, matching the normal record path where
    //! the player stays stopped (so the record button etc. are not treated as playback)
    EXPECT_FALSE(m_controller->isPlaying());
}

TEST_F(PlaybackControllerTests, CanReceiveAction_WhileRecording_BlocksPlayStopButNotPlayPause)
{
    //! [GIVEN] Recording is running
    setRecording(true);

    //! [THEN] Space and Shift+Space are blocked, the toolbar button is not
    EXPECT_FALSE(m_controller->canReceiveAction("action://playback/toggle-play-stop"));
    EXPECT_FALSE(m_controller->canReceiveAction("action://playback/toggle-play-from-cursor"));
    EXPECT_TRUE(m_controller->canReceiveAction("action://playback/toggle-play-pause"));
}

TEST_F(PlaybackControllerTests, CanReceiveAction_WhileNotRecording_AllowsAllTogglePlayActions)
{
    //! [GIVEN] Not recording
    setRecording(false);

    //! [THEN] All toggle-play actions are available
    EXPECT_TRUE(m_controller->canReceiveAction("action://playback/toggle-play-stop"));
    EXPECT_TRUE(m_controller->canReceiveAction("action://playback/toggle-play-from-cursor"));
    EXPECT_TRUE(m_controller->canReceiveAction("action://playback/toggle-play-pause"));
}
}
