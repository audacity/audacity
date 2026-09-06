/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "../internal/recordcontroller.h"

#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "playback/tests/mocks/playbackcontrollermock.h"
#include "record/recorderrors.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "trackedit/tests/mocks/tracksinteractionmock.h"

#include "mocks/recordmock.h"

using ::testing::_;
using ::testing::InSequence;
using ::testing::NiceMock;
using ::testing::Return;

namespace au::record {
class RecordControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = std::make_shared<RecordController>(muse::modularity::globalCtx());

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_playbackController = std::make_shared<NiceMock<playback::PlaybackControllerMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_record = std::make_shared<NiceMock<RecordMock> >();
        m_audioDriverController
            = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_tracksInteraction
            = std::make_shared<NiceMock<trackedit::TracksInteractionMock> >();
        m_trackeditInteraction
            = std::make_shared<NiceMock<trackedit::TrackeditInteractionMock> >();
        m_trackNavigationController
            = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();

        m_controller->globalContext.set(m_globalContext);
        m_controller->playbackController.set(m_playbackController);
        m_controller->selectionController.set(m_selectionController);
        m_controller->interactive.set(m_interactive);
        m_controller->record.set(m_record);
        m_controller->audioDriverController.set(m_audioDriverController);
        m_controller->tracksInteraction.set(m_tracksInteraction);
        m_controller->trackeditInteraction.set(m_trackeditInteraction);
        m_controller->trackNavigationController.set(m_trackNavigationController);

        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));

        ON_CALL(*m_record, start())
        .WillByDefault(Return(muse::make_ok()));
        ON_CALL(*m_record, leadInRecording())
        .WillByDefault(Return(muse::make_ok()));
    }

    // The controller's action handlers are private; tests reach them through
    // the fixture's friendship
    void leadInRecording()
    {
        m_controller->leadInRecording();
    }

    void toggleRecord()
    {
        m_controller->toggleRecord();
    }

    void pause()
    {
        m_controller->pause();
    }

    void startWithNewTrack()
    {
        m_controller->startWithNewTrack();
    }

    bool recordStatusIsRunning() const
    {
        return m_controller->m_currentRecordStatus == RecordController::RecordStatus::Running;
    }

    std::shared_ptr<RecordController> m_controller;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;
    std::shared_ptr<playback::PlaybackControllerMock> m_playbackController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<muse::InteractiveMock> m_interactive;
    std::shared_ptr<RecordMock> m_record;
    std::shared_ptr<NiceMock<audio::AudioDriverControllerMock> > m_audioDriverController;
    std::shared_ptr<NiceMock<trackedit::TracksInteractionMock> > m_tracksInteraction;
    std::shared_ptr<NiceMock<trackedit::TrackeditInteractionMock> > m_trackeditInteraction;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavigationController;
};

TEST_F(RecordControllerTests, LeadInRecordingStartsFromPlayheadNotSelectionStart)
{
    //! [GIVEN] The playhead is at 8s while the last selection (e.g. from a previous
    //! recording) started at 5s
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(8.0)));
    ON_CALL(*m_selectionController, selectionStartTime())
    .WillByDefault(Return(trackedit::secs_t(5.0)));

    //! [WHEN] Lead-in recording is initiated
    leadInRecording();

    //! [THEN] The recording start position is the current playhead position
    EXPECT_DOUBLE_EQ(m_controller->leadInRecordingStartTime().to_double(), 8.0);
}

TEST_F(RecordControllerTests, LeadInRecordingFromPausedPlaybackStopsPlaybackFirst)
{
    //! [GIVEN] Playback is paused
    ON_CALL(*m_playbackController, isPaused())
    .WillByDefault(Return(true));

    //! [THEN] Playback is stopped before the lead-in recording starts, so that
    //! recording begins immediately (a paused engine would otherwise stay paused)
    InSequence seq;
    EXPECT_CALL(*m_playbackController, stop());
    EXPECT_CALL(*m_record, leadInRecording())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] Lead-in recording is initiated
    leadInRecording();
}

TEST_F(RecordControllerTests, RecordFromPausedPlaybackStopsPlaybackFirst)
{
    //! [GIVEN] Playback is paused
    ON_CALL(*m_playbackController, isPaused())
    .WillByDefault(Return(true));

    //! [THEN] Playback is stopped before recording starts, so that recording
    //! begins immediately from the current playhead position
    InSequence seq;
    EXPECT_CALL(*m_playbackController, stop());
    EXPECT_CALL(*m_record, start())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] Recording is initiated
    toggleRecord();
}

TEST_F(RecordControllerTests, RecordFromStoppedPlaybackDoesNotStopPlayback)
{
    //! [GIVEN] Playback is already stopped
    ON_CALL(*m_playbackController, isPaused())
    .WillByDefault(Return(false));

    //! [THEN] Playback is not stopped before recording starts
    EXPECT_CALL(*m_playbackController, stop())
    .Times(0);
    EXPECT_CALL(*m_record, start())
    .WillOnce(Return(muse::make_ok()));

    //! [WHEN] Recording is initiated
    toggleRecord();
}

TEST_F(RecordControllerTests, ToggleRecordWhilePausedResumesRecording)
{
    //! [GIVEN] A recording is running and has been paused
    ON_CALL(*m_record, pause())
    .WillByDefault(Return(muse::make_ok()));
    toggleRecord();
    pause();

    //! [THEN] The recording is resumed, not stopped
    EXPECT_CALL(*m_record, resume())
    .WillOnce(Return(muse::make_ok()));
    EXPECT_CALL(*m_record, stop())
    .Times(0);

    //! [WHEN] Record is triggered again
    toggleRecord();

    //! [THEN] The recording is running again
    EXPECT_TRUE(recordStatusIsRunning());
}

TEST_F(RecordControllerTests, PauseWhilePausedResumesRecording)
{
    //! [GIVEN] A recording is running and has been paused
    ON_CALL(*m_record, pause())
    .WillByDefault(Return(muse::make_ok()));
    toggleRecord();
    pause();

    //! [THEN] The recording is resumed rather than paused again
    EXPECT_CALL(*m_record, resume())
    .WillOnce(Return(muse::make_ok()));
    EXPECT_CALL(*m_record, pause())
    .Times(0);

    //! [WHEN] Pause is triggered again
    pause();

    //! [THEN] The recording is running again
    EXPECT_TRUE(recordStatusIsRunning());
}

class RecordOnNewTrackWithoutInputsTests : public RecordControllerTests, public ::testing::WithParamInterface<bool>
{
};

TEST_P(RecordOnNewTrackWithoutInputsTests, ReportsErrorWithoutChangingTracksOrStartingRecording)
{
    const bool playbackPaused = GetParam();
    audio::AudioConfiguration configuration;
    configuration.inputChannelSelection.clear();
    ON_CALL(*m_audioDriverController, configuration())
    .WillByDefault(Return(configuration));
    ON_CALL(*m_playbackController, isPaused())
    .WillByDefault(Return(playbackPaused));

    EXPECT_CALL(*m_tracksInteraction, addWaveTrack(_)).Times(0);
    EXPECT_CALL(*m_trackeditInteraction, deleteTracks(_)).Times(0);
    EXPECT_CALL(*m_selectionController, setSelectedTracks(_, _)).Times(0);
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(_, _)).Times(0);
    EXPECT_CALL(*m_record, start()).Times(0);

    InSequence sequence;
    EXPECT_CALL(*m_playbackController, stop()).Times(playbackPaused ? 1 : 0);
    EXPECT_CALL(*m_interactive, error(muse::trc("record", "Recording error"),
                                      ::testing::Field(&muse::IInteractive::Text::text, make_ret(Err::NoRecordingDevice).text()),
                                      _, _, _, _))
    .WillOnce(Return(muse::async::make_promise<muse::IInteractive::Result>(
                         [](const auto& resolve) {
        return resolve(muse::IInteractive::Result {});
    }, muse::async::PromiseType::AsyncByBody)));

    startWithNewTrack();

    EXPECT_FALSE(m_controller->isRecording());
}

INSTANTIATE_TEST_SUITE_P(
    PlaybackStates,
    RecordOnNewTrackWithoutInputsTests,
    ::testing::Values(false, true),
    [](const ::testing::TestParamInfo<bool>& info) {
    return info.param ? "Paused" : "Stopped";
});

TEST_F(RecordControllerTests, RecordOnNewTrackCreatesOneTrackPerInputGroup)
{
    audio::AudioConfiguration configuration;
    configuration.inputChannelSelection = { { { 0 } }, { { 2, 3 } } };
    ON_CALL(*m_audioDriverController, configuration())
    .WillByDefault(Return(configuration));

    InSequence sequence;
    EXPECT_CALL(*m_tracksInteraction, addWaveTrack(1)).WillOnce(Return(101));
    EXPECT_CALL(*m_tracksInteraction, addWaveTrack(2)).WillOnce(Return(102));
    EXPECT_CALL(*m_selectionController,
                setSelectedTracks(trackedit::TrackIdList { 101, 102 }, true));
    EXPECT_CALL(*m_trackNavigationController, setFocusedTrack(101, false));
    EXPECT_CALL(*m_record, start()).WillOnce(Return(muse::make_ok()));

    startWithNewTrack();

    EXPECT_TRUE(recordStatusIsRunning());
}

TEST_F(RecordControllerTests, RecordOnNewTrackFailureDeletesEveryCreatedTrack)
{
    audio::AudioConfiguration configuration;
    configuration.inputChannelSelection = { { { 0 } }, { { 2, 3 } } };
    ON_CALL(*m_audioDriverController, configuration())
    .WillByDefault(Return(configuration));
    ON_CALL(*m_tracksInteraction, addWaveTrack(1)).WillByDefault(Return(101));
    ON_CALL(*m_tracksInteraction, addWaveTrack(2)).WillByDefault(Return(102));
    ON_CALL(*m_record, start())
    .WillByDefault(Return(muse::make_ret(muse::Ret::Code::UnknownError)));

    EXPECT_CALL(*m_trackeditInteraction,
                deleteTracks(trackedit::TrackIdList { 101, 102 }))
    .WillOnce(Return(true));
    EXPECT_CALL(*m_interactive, error(_, _, _, _, _, _))
    .WillOnce(Return(muse::async::make_promise<muse::IInteractive::Result>(
                         [](const auto& resolve) {
        return resolve(muse::IInteractive::Result {});
    }, muse::async::PromiseType::AsyncByBody)));

    startWithNewTrack();

    EXPECT_FALSE(recordStatusIsRunning());
}
}
