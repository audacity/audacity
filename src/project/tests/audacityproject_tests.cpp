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
#include "project/projecterrors.h"
#include "project/internal/audacityproject.h"

#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"

namespace au::project {
class Project_Audacity4ProjectTests : public ::testing::Test
{
protected:
    Audacity4Project* m_currentProject = nullptr;
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> m_trackeditProjectCreator;
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> m_projectViewStateCreator;

    void SetUp() override
    {
        m_currentProject = new project::Audacity4Project();
        m_currentProject->trackeditProjectCreator.set(m_trackeditProjectCreator);
        m_currentProject->viewStateCreator.set(m_projectViewStateCreator);
    }

    void TearDown() override
    {
        // delete m_currentProject;
    }
};

TEST_F(Project_Audacity4ProjectTests, Load_ValidFile_ReturnsSuccess)
{
    muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3";

    EXPECT_EQ(muse::io::FileInfo::exists(testPath), true);

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
}

TEST_F(Project_Audacity4ProjectTests, Load_FileDoesNotExist_ReturnsProjectFileNotFound)
{
    muse::io::path_t testPath = "/nonexistent/project.aup3";

    // Ensure the path truly doesn’t exist
    ASSERT_FALSE(muse::io::FileInfo::exists(testPath));

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

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
    muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_readprotected.aup3";

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsReadProtected));
    EXPECT_TRUE(ret.data<std::string>("body").has_value());
}

TEST_F(Project_Audacity4ProjectTests, Load_EmptyFileIsWriteProtected_ReturnsSuccess)
{
    muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_writeprotected.aup3";

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
}

TEST_F(Project_Audacity4ProjectTests, Load_NonEmptyFileIsWriteProtected_ReturnsWriteProtected)
{
    muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test_writeprotected.aup3";

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsWriteProtected));
    EXPECT_TRUE(ret.data<std::string>("body").has_value());
}
} // namespace au::project
