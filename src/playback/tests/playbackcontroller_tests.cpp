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
        .WillOnce(Return(false));

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
TEST_F(PlaybackControllerTests, TogglePlay_WhenStopped)
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
}
