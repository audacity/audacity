/*
* SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2025 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "global/io/fileinfo.h"
#include "global/types/ret.h"
#include "project/internal/audacityproject.h"
#include "project/projecterrors.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "playback/tests/mocks/playbackmock.h"
#include "playback/tests/mocks/playermock.h"
#include "record/tests/mocks/recordcontrollermock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

#include "project/internal/projectactionscontroller.h"
#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"

using namespace muse;

using namespace muse;
using namespace au;
using namespace au::project;
using namespace au::context;
using namespace au::playback;
using namespace testing;

namespace au::project {
class Project_Audacity4ProjectTests : public ::testing::Test
{
protected:
    std::shared_ptr<ProjectActionsController> m_actionsController;
    std::shared_ptr<ApplicationMock> m_application;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<actions::IActionsDispatcher> m_dispatcher;
    std::shared_ptr<record::RecordControllerMock> m_recordController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> m_trackeditProjectCreator;
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> m_projectViewStateCreator;
    // std::shared_ptr<project::AudacityProjectMock>
    Audacity4Project* m_currentProject = nullptr;

    std::shared_ptr<PlaybackMock> m_playback;
    std::shared_ptr<PlayerMock> m_player;

    void SetUp() override
    {
        // m_controller = new PlaybackController();
        m_actionsController = std::make_shared<ProjectActionsController>();
        // m_actionsController->init();

        m_application = std::make_shared<muse::ApplicationMock>();
        // m_controller->application.set(m_application);

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        // m_controller->globalContext.set(m_globalContext);
        // m_actionsController->globalContext.set(m_globalContext);

        m_dispatcher = std::make_shared<actions::ActionsDispatcherMock>();
        // m_controller->dispatcher.set(m_dispatcher);

        m_recordController = std::make_shared<record::RecordControllerMock>();
        // m_controller->recordController.set(m_recordController);

        m_selectionController = std::make_shared<trackedit::SelectionControllerMock>();
        // m_controller->selectionController.set(m_selectionController);

        m_trackeditProject = std::make_shared<trackedit::TrackeditProjectMock>();

        // m_currentProject = std::make_shared<project::AudacityProjectMock>();
        // m_currentProject = std::make_shared<project::Audacity4Project>();
        m_currentProject = new project::Audacity4Project();
        m_currentProject->trackeditProjectCreator.set(m_trackeditProjectCreator);
        m_currentProject->viewStateCreator.set(m_projectViewStateCreator);

        // ON_CALL(*m_currentProject, trackeditProjectCreator())
        // .WillByDefault(Return(m_trackeditProjectCreator));

        m_playback = std::make_shared<PlaybackMock>();
        // m_controller->playback.set(m_playback);

        m_player = std::make_shared<PlayerMock>();

        // EXPECT_CALL(*m_playback, player(_))
        // .WillOnce(Return(m_player));
        //
        // EXPECT_CALL(*m_globalContext, setPlayer(_))
        // .Times(1);

        // ON_CALL(*m_globalContext, currentProject())
        // .WillByDefault(Return(m_currentProject));

        // ON_CALL(*m_currentProject, trackeditProject())
        // .WillByDefault(Return(m_trackeditProject));

        ON_CALL(*m_trackeditProject, totalTime())
        .WillByDefault(Return(100));

        // EXPECT_CALL(*m_recordController, isRecording())
        // .WillRepeatedly(Return(false));

        // m_controller->init();
    }

    void TearDown() override
    {
        // delete m_controller;
    }
};

TEST_F(Project_Audacity4ProjectTests, Load_ValidFile_ReturnsSuccess)
{
    io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3";

    EXPECT_EQ(io::FileInfo::exists(testPath), true);

    const Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
}

TEST_F(Project_Audacity4ProjectTests, Load_FileDoesNotExist_ReturnsProjectFileNotFound)
{
    io::path_t testPath = "/nonexistent/project.aup3";

    // Ensure the path truly doesn’t exist
    ASSERT_FALSE(io::FileInfo::exists(testPath));

    const Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileNotFound));

    auto title = ret.data<std::string>("title");
    auto body  = ret.data<std::string>("body");

    ASSERT_TRUE(title.has_value());
    ASSERT_TRUE(body.has_value());
    EXPECT_THAT(title.value(), ::testing::HasSubstr("Cannot read file"));
}

TEST_F(Project_Audacity4ProjectTests, Load_FileCannotBeOpened_ReturnsReadProtected)
{
    io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_readprotected.aup3";

    const Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsReadProtected));
    EXPECT_TRUE(ret.data<std::string>("body").has_value());
}

TEST_F(Project_Audacity4ProjectTests, Load_EmptyFileIsWriteProtected_ReturnsSuccess)
{
    io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_writeprotected.aup3";

    const Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
}

TEST_F(Project_Audacity4ProjectTests, Load_NonEmptyFileIsWriteProtected_ReturnsWriteProtected)
{
    io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test_writeprotected.aup3";

    const Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsWriteProtected));
    EXPECT_TRUE(ret.data<std::string>("body").has_value());
}
} // namespace au::project
