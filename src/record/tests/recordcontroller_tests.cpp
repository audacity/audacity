/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "../internal/recordcontroller.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "playback/tests/mocks/playbackcontrollermock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "interactive/tests/mocks/interactivemock.h"

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

        m_controller->globalContext.set(m_globalContext);
        m_controller->playbackController.set(m_playbackController);
        m_controller->selectionController.set(m_selectionController);
        m_controller->interactive.set(m_interactive);
        m_controller->record.set(m_record);

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

    std::shared_ptr<RecordController> m_controller;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;
    std::shared_ptr<playback::PlaybackControllerMock> m_playbackController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<muse::InteractiveMock> m_interactive;
    std::shared_ptr<RecordMock> m_record;
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
}
