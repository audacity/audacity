/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "async/processevents.h"

#include "../internal/projectactionscontroller.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "rcommand/tests/mocks/commanddispatchermock.h"
#include "appshell/appshellcommands.h"
#include "record/tests/mocks/recordcontrollermock.h"

#include "mocks/audacityprojectmock.h"
#include "mocks/au3audiocomservicemock.h"
#include "mocks/cloudprojectsprovidermock.h"
#include "mocks/recentfilescontrollermock.h"

using ::testing::_;
using ::testing::InSequence;
using ::testing::NiceMock;
using ::testing::Return;
using ::testing::Truly;

using Button = muse::IInteractive::Button;

namespace au::project {
//! Closing a project, or quitting, while it has unsaved changes: the prompt, and what it does
//! with a recording that is still running
class ProjectActionsControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = std::make_shared<ProjectActionsController>();

        m_project = std::make_shared<NiceMock<AudacityProjectMock> >();
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_recordController = std::make_shared<NiceMock<record::RecordControllerMock> >();
        m_recentFiles = std::make_shared<NiceMock<RecentFilesControllerMock> >();
        m_cloudProjects = std::make_shared<NiceMock<au3cloud::CloudProjectsProviderMock> >();
        m_commandDispatcher = std::make_shared<NiceMock<muse::rcommand::CommandDispatcherMock> >();

        m_controller->globalContext.set(m_globalContext);
        m_controller->interactive.set(m_interactive);
        m_controller->recordController.set(m_recordController);
        m_controller->recentFilesController.set(m_recentFiles);
        m_controller->cloudProjectsProvider.set(m_cloudProjects);
        m_controller->commandDispatcher.set(m_commandDispatcher);
        m_controller->audioComService.set(std::make_shared<NiceMock<au3cloud::Au3AudioComServiceMock> >());

        //! A saved, local project with unsaved changes, while recording
        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_project, hasUnsavedChanges()).WillByDefault(Return(true));
        ON_CALL(*m_project, isNewlyCreated()).WillByDefault(Return(false));
        ON_CALL(*m_project, isCloudProject()).WillByDefault(Return(false));
        ON_CALL(*m_project, save(_, _)).WillByDefault(Return(muse::make_ok()));
        ON_CALL(*m_recordController, isRecording()).WillByDefault(Return(true));
        ON_CALL(*m_recordController, stopRecording()).WillByDefault(Return(muse::make_ok()));

        //! Asking with standard buttons goes through it
        ON_CALL(*m_interactive, buttonData(_)).WillByDefault([](Button b) {
            return muse::IInteractive::ButtonData(int(b), "");
        });
        //! Closing without quitting goes to the home page, which needs no opening
        ON_CALL(*m_interactive, isOpened(::testing::A<const muse::Uri&>()))
        .WillByDefault(Return(muse::RetVal<bool>::make_ok(true)));
        //! Quitting the app goes through a command
        ON_CALL(*m_commandDispatcher, dispatch(_)).WillByDefault([](const muse::rcommand::Request& request) {
            return muse::async::make_promise<muse::rcommand::Response>([request](auto resolve) {
                return resolve(muse::rcommand::make_response(request, muse::make_ok()));
            });
        });
    }

    //! Closes the project and, if the user is asked about saving, waits for the answer: whether it was closed
    bool closeOpenedProject(bool quitApp = false)
    {
        auto closedLater = std::make_shared<bool>(false);
        if (m_controller->closeOpenedProject(quitApp, [closedLater]() { *closedLater = true; })) {
            return true;
        }
        for (int i = 0; i < 100; ++i) {
            muse::async::processMessages();
        }
        return *closedLater;
    }

    static ::testing::Matcher<const muse::rcommand::Request&> IsQuitApp()
    {
        return Truly([](const muse::rcommand::Request& request) {
            return request.command == appshell::APP_QUIT_COMMAND;
        });
    }

    //! The answers to the warnings, in the order they are asked
    template<typename ... Buttons>
    void answer(Buttons... buttons)
    {
        auto& expectation = EXPECT_CALL(*m_interactive, warning(_, _, _, _, _, _))
                            .Times(sizeof...(buttons));
        (expectation.WillOnce([btn = int(buttons)] {
            return muse::async::make_promise<muse::IInteractive::Result>([btn](auto resolve, auto) {
                return resolve(muse::IInteractive::Result(btn));
            });
        }), ...);
    }

    std::shared_ptr<ProjectActionsController> m_controller;
    std::shared_ptr<AudacityProjectMock> m_project;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<muse::InteractiveMock> m_interactive;
    std::shared_ptr<record::RecordControllerMock> m_recordController;
    std::shared_ptr<RecentFilesControllerMock> m_recentFiles;
    std::shared_ptr<au3cloud::CloudProjectsProviderMock> m_cloudProjects;
    std::shared_ptr<muse::rcommand::CommandDispatcherMock> m_commandDispatcher;
};

TEST_F(ProjectActionsControllerTests, QuitWhileRecording_AsksAboutSaving)
{
    //! [THEN] The user is asked whether to save, and cancelling keeps everything as it was
    //! (no stop recording, no save, no close)
    answer(Button::Cancel);
    EXPECT_CALL(*m_recordController, stopRecording()).Times(0);
    EXPECT_CALL(*m_project, save(_, _)).Times(0);
    EXPECT_CALL(*m_project, close()).Times(0);
    EXPECT_CALL(*m_commandDispatcher, dispatch(IsQuitApp())).Times(0);

    //! [WHEN] Quitting
    EXPECT_FALSE(closeOpenedProject(true /*quitApp*/));
}

TEST_F(ProjectActionsControllerTests, SaveWhileRecording_StopsRecordingBeforeSaving)
{
    answer(Button::Save);

    //! [THEN] The recording is stopped first, or what is being recorded would not be saved
    {
        InSequence seq;
        EXPECT_CALL(*m_recordController, stopRecording()).Times(1);
        EXPECT_CALL(*m_project, save(_, _)).Times(1);
        EXPECT_CALL(*m_project, close()).Times(1);
        EXPECT_CALL(*m_commandDispatcher, dispatch(IsQuitApp())).Times(1);
    }

    //! [WHEN] Quitting and choosing to save
    EXPECT_TRUE(closeOpenedProject(true /*quitApp*/));
}

TEST_F(ProjectActionsControllerTests, DontSaveWhileRecording_ClosesWithoutSaving)
{
    answer(Button::DontSave);

    //! [THEN] Nothing is saved; the recording is stopped, then the project is closed
    EXPECT_CALL(*m_project, save(_, _)).Times(0);
    {
        InSequence seq;
        EXPECT_CALL(*m_recordController, stopRecording()).Times(1);
        EXPECT_CALL(*m_project, close()).Times(1);
    }

    //! [WHEN] Closing and choosing not to save
    EXPECT_TRUE(closeOpenedProject());
}

TEST_F(ProjectActionsControllerTests, DontSaveWhenRecordingCannotBeStopped_KeepsProjectOpen)
{
    //! [GIVEN] The recording cannot be stopped
    ON_CALL(*m_recordController, stopRecording()).WillByDefault(Return(muse::make_ret(muse::Ret::Code::UnknownError)));

    //! [THEN] The project is not closed under the recording
    answer(Button::DontSave);
    EXPECT_CALL(*m_project, close()).Times(0);

    //! [WHEN] Closing and choosing not to save
    EXPECT_FALSE(closeOpenedProject());
}

TEST_F(ProjectActionsControllerTests, SaveWhenRecordingCannotBeStopped_KeepsProjectOpen)
{
    //! [GIVEN] The recording cannot be stopped
    ON_CALL(*m_recordController, stopRecording()).WillByDefault(Return(muse::make_ret(muse::Ret::Code::UnknownError)));

    //! [THEN] Nothing is saved without the recording, and the project stays open, so the recording goes on
    answer(Button::Save);
    EXPECT_CALL(*m_project, save(_, _)).Times(0);
    EXPECT_CALL(*m_project, close()).Times(0);

    //! [WHEN] Closing and choosing to save
    EXPECT_FALSE(closeOpenedProject());
}

TEST_F(ProjectActionsControllerTests, SaveWhenNotRecording_DoesNotStopRecording)
{
    //! [GIVEN] Nothing is being recorded in this project (another window may be playing or recording)
    ON_CALL(*m_recordController, isRecording()).WillByDefault(Return(false));

    //! [THEN] Stopping is not attempted, as it would fail; the project is saved and closed
    answer(Button::Save);
    EXPECT_CALL(*m_recordController, stopRecording()).Times(0);
    EXPECT_CALL(*m_project, save(_, _)).Times(1);
    EXPECT_CALL(*m_project, close()).Times(1);

    //! [WHEN] Closing and choosing to save
    EXPECT_TRUE(closeOpenedProject());
}

TEST_F(ProjectActionsControllerTests, SaveFails_KeepsProjectOpen)
{
    //! [GIVEN] Saving fails
    ON_CALL(*m_project, save(_, _)).WillByDefault(Return(muse::make_ret(muse::Ret::Code::UnknownError)));
    answer(Button::Save);

    //! [THEN] The project stays open, so nothing is lost
    EXPECT_CALL(*m_project, close()).Times(0);

    //! [WHEN] Closing and choosing to save
    EXPECT_FALSE(closeOpenedProject());
}

TEST_F(ProjectActionsControllerTests, NoUnsavedChanges_ClosesWithoutAsking)
{
    //! [GIVEN] Everything is saved
    ON_CALL(*m_project, hasUnsavedChanges()).WillByDefault(Return(false));

    //! [THEN] No question, the project is closed
    answer();
    EXPECT_CALL(*m_project, close()).Times(1);

    //! [WHEN] Closing
    EXPECT_TRUE(closeOpenedProject());
}
}
