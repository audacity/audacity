/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "playback/tests/mocks/playbackcontrollermock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/recordconfigurationmock.h"
#include "mocks/recordmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "trackedit/tests/mocks/tracksinteractionmock.h"

#include "../internal/recordcontroller.h"

using ::testing::_;
using ::testing::NiceMock;
using ::testing::Return;

using namespace muse;
using namespace au;
using namespace au::record;

static const actions::ActionCode RECORD_START_CODE("action://record/start");
static const actions::ActionCode RECORD_STOP_CODE("action://record/stop");
static const actions::ActionCode RECORD_LEAD_IN_CODE("action://record/lead-in-recording");
static const actions::ActionCode RECORD_ON_CURRENT_TRACK_CODE("record-on-current-track");
static const actions::ActionCode RECORD_ON_NEW_TRACK_CODE("record-on-new-track");

namespace au::record {
class RecordControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = new RecordController(muse::modularity::globalCtx());

        m_configuration = std::make_shared<NiceMock<RecordConfigurationMock> >();
        m_controller->configuration.set(m_configuration);

        m_audioDriverController = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_controller->audioDriverController.set(m_audioDriverController);

        m_dispatcher = std::make_shared<NiceMock<actions::ActionsDispatcherMock> >();
        m_controller->dispatcher.set(m_dispatcher);

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_controller->globalContext.set(m_globalContext);

        m_interactive = std::make_shared<NiceMock<InteractiveMock> >();
        m_controller->interactive.set(m_interactive);

        m_record = std::make_shared<NiceMock<RecordMock> >();
        m_controller->record.set(m_record);

        m_playbackController = std::make_shared<NiceMock<playback::PlaybackControllerMock> >();
        m_controller->playbackController.set(m_playbackController);

        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_controller->selectionController.set(m_selectionController);

        m_tracksInteraction = std::make_shared<NiceMock<trackedit::TracksInteractionMock> >();
        m_controller->tracksInteraction.set(m_tracksInteraction);

        m_trackeditInteraction = std::make_shared<NiceMock<trackedit::TrackeditInteractionMock> >();
        m_controller->trackeditInteraction.set(m_trackeditInteraction);

        m_trackNavigationController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_controller->trackNavigationController.set(m_trackNavigationController);

        m_currentProject = std::make_shared<project::AudacityProjectMock>();

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        ON_CALL(*m_playbackController, isPlaying())
        .WillByDefault(Return(false));

        ON_CALL(*m_playbackController, isPlayingChanged())
        .WillByDefault(Return(m_isPlayingChanged));

        //! NOTE: use persistent channels so tests can inject record events
        ON_CALL(*m_record, recordPositionChanged())
        .WillByDefault(Return(m_recordPositionChanged));

        ON_CALL(*m_record, recordingFinished())
        .WillByDefault(Return(m_recordingFinished));

        m_controller->init();
    }

    void TearDown() override
    {
        delete m_controller;
    }

    //! NOTE: toolbar Record button
    void toggleRecord()
    {
        m_controller->toggleRecord();
    }

    void pauseRecord()
    {
        m_controller->pause();
    }

    void stopRecord()
    {
        m_controller->stop();
    }

    void setPlaying(bool isPlaying)
    {
        ON_CALL(*m_playbackController, isPlaying())
        .WillByDefault(Return(isPlaying));
    }

    //! NOTE: brings the controller into the Running record state
    void startRecordingSuccessfully()
    {
        EXPECT_CALL(*m_record, start())
        .WillOnce(Return(muse::make_ok()));

        toggleRecord();

        ASSERT_TRUE(m_controller->isRecording());
    }

    void expectErrorDialog()
    {
        EXPECT_CALL(*m_interactive, error(_, _, _, _, _, _))
        .WillOnce(Return(muse::async::make_promise<muse::IInteractive::Result>(
                             [](const auto& resolve) { return resolve(muse::IInteractive::Result {}); },
                             muse::async::PromiseType::AsyncByBody)));
    }

    RecordController* m_controller = nullptr;

    std::shared_ptr<RecordConfigurationMock> m_configuration;
    std::shared_ptr<audio::AudioDriverControllerMock> m_audioDriverController;
    std::shared_ptr<actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<InteractiveMock> m_interactive;
    std::shared_ptr<RecordMock> m_record;
    std::shared_ptr<playback::PlaybackControllerMock> m_playbackController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TracksInteractionMock> m_tracksInteraction;
    std::shared_ptr<trackedit::TrackeditInteractionMock> m_trackeditInteraction;
    std::shared_ptr<trackedit::TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    muse::async::Notification m_isPlayingChanged;
    muse::async::Channel<muse::secs_t> m_recordPositionChanged;
    muse::async::Notification m_recordingFinished;
};

/**
 * @brief Start recording from the stopped state
 * @details User pressed record while nothing is playing or recording
 *          The recorder starts; the player is not touched
 */
TEST_F(RecordControllerTests, StartRecord_FromStopped_StartsRecording)
{
    //! [THEN] The recorder is started, the player is not touched
    EXPECT_CALL(*m_record, start())
    .WillOnce(Return(muse::make_ok()));
    EXPECT_CALL(*m_playbackController, stop())
    .Times(0);

    //! [WHEN] The user presses record
    toggleRecord();

    //! [THEN] The controller reports that it is recording
    EXPECT_TRUE(m_controller->isRecording());
}

/**
 * @brief Start recording fails
 * @details The recorder reports an error on start
 *          An error dialog is shown and the controller stays stopped
 */
TEST_F(RecordControllerTests, StartRecord_WhenStartFails_ReportsErrorAndStaysStopped)
{
    //! [GIVEN] The recorder fails to start
    EXPECT_CALL(*m_record, start())
    .WillOnce(Return(muse::make_ret(muse::Ret::Code::InternalError)));

    //! [THEN] An error dialog is shown
    expectErrorDialog();

    //! [WHEN] The user presses record
    toggleRecord();

    //! [THEN] The controller is not recording
    EXPECT_FALSE(m_controller->isRecording());
}

/**
 * @brief Pause while recording
 * @details User pressed pause while the recorder is running
 *          The recorder is paused; the controller still reports recording
 */
TEST_F(RecordControllerTests, Pause_WhileRecording_PausesRecorder)
{
    //! [GIVEN] A recording is running
    startRecordingSuccessfully();

    //! [THEN] The recorder is paused
    EXPECT_CALL(*m_record, pause())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] The user presses pause
    pauseRecord();

    //! [THEN] A paused recording still counts as recording
    EXPECT_TRUE(m_controller->isRecording());
}

/**
 * @brief Stop while recording
 * @details User pressed stop while the recorder is running
 *          The recorder is stopped; playback is not restarted
 */
TEST_F(RecordControllerTests, Stop_WhileRecording_StopsWithoutRestartingPlayback)
{
    //! [GIVEN] A recording is running
    startRecordingSuccessfully();

    //! [THEN] The recorder is stopped
    EXPECT_CALL(*m_record, stop())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] The user presses stop
    stopRecord();

    //! [THEN] The controller is no longer recording
    EXPECT_FALSE(m_controller->isRecording());
}

/**
 * @brief Toggle record off while recording
 * @details User pressed the record button while the recorder is running
 *          The recording is stopped
 */
TEST_F(RecordControllerTests, ToggleRecordOff_WhileRunning_StopsRecording)
{
    //! [GIVEN] A recording is running
    startRecordingSuccessfully();

    //! [THEN] The recorder is stopped
    EXPECT_CALL(*m_record, stop())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] The user presses record again
    toggleRecord();

    //! [THEN] The controller is no longer recording
    EXPECT_FALSE(m_controller->isRecording());
}

/**
 * @brief The recording finishes externally
 * @details The record stream ended on its own (e.g. the engine finished)
 *          The controller leaves the recording state without stopping the recorder again
 */
TEST_F(RecordControllerTests, RecordingFinished_External_SetsStopped)
{
    //! [GIVEN] A recording is running
    startRecordingSuccessfully();

    //! [THEN] The recorder is not stopped by the controller
    EXPECT_CALL(*m_record, stop())
    .Times(0);

    //! [WHEN] The recording finished notification arrives
    m_recordingFinished.notify();

    //! [THEN] The controller is no longer recording
    EXPECT_FALSE(m_controller->isRecording());
}

/**
 * @brief Record actions availability while playing
 * @details Playback is running: starting a recording is currently not possible
 */
TEST_F(RecordControllerTests, CanReceiveAction_RecordStart_BlockedWhilePlaying)
{
    //! [GIVEN] Playback is running
    setPlaying(true);

    //! [THEN] The record actions are blocked
    EXPECT_FALSE(m_controller->canReceiveAction(RECORD_START_CODE));
    EXPECT_FALSE(m_controller->canReceiveAction(RECORD_ON_CURRENT_TRACK_CODE));
    EXPECT_FALSE(m_controller->canReceiveAction(RECORD_ON_NEW_TRACK_CODE));
    EXPECT_FALSE(m_controller->canReceiveAction(RECORD_LEAD_IN_CODE));
}

/**
 * @brief Record actions availability when stopped
 * @details Nothing is playing: recording can be started
 */
TEST_F(RecordControllerTests, CanReceiveAction_RecordStart_AllowedWhenStopped)
{
    //! [GIVEN] Playback is stopped (fixture default)

    //! [THEN] The record actions are available
    EXPECT_TRUE(m_controller->canReceiveAction(RECORD_START_CODE));
    EXPECT_TRUE(m_controller->canReceiveAction(RECORD_ON_CURRENT_TRACK_CODE));
    EXPECT_TRUE(m_controller->canReceiveAction(RECORD_ON_NEW_TRACK_CODE));
    EXPECT_TRUE(m_controller->canReceiveAction(RECORD_LEAD_IN_CODE));
}

/**
 * @brief Stop action availability
 * @details The record stop action is available only while recording
 */
TEST_F(RecordControllerTests, CanReceiveAction_RecordStop_OnlyWhileRecording)
{
    //! [GIVEN] No recording in progress
    EXPECT_FALSE(m_controller->canReceiveAction(RECORD_STOP_CODE));

    //! [WHEN] A recording is running
    startRecordingSuccessfully();

    //! [THEN] The stop action becomes available
    EXPECT_TRUE(m_controller->canReceiveAction(RECORD_STOP_CODE));
}

/**
 * @brief isRecordAllowed depends on playback
 * @details Recording is currently not allowed while playback is running
 */
TEST_F(RecordControllerTests, IsRecordAllowed_DependsOnPlaybackState)
{
    //! [GIVEN] Playback is stopped (fixture default)
    EXPECT_TRUE(m_controller->isRecordAllowed());

    //! [WHEN] Playback is running
    setPlaying(true);

    //! [THEN] Recording is not allowed
    EXPECT_FALSE(m_controller->isRecordAllowed());
}
}
