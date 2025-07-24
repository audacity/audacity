/*
* SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity Limited
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
#include "trackedit/tests/mocks/clipboardmock.h"

#include "testtools.h"

namespace au::project {
enum class AccessMode
{
    ReadProtected,
    WriteProtected
};

// @brief RAII wrapper for a temporary test file that is copied from a source and optionally made read/write protected.
class ScopedTestFile
{
public:
    ScopedTestFile(const std::string& source, const std::string& destination, const AccessMode mode)
        : m_destinationPath(destination)
    {
        assert(source != destination && "ScopedTestFile: source and destination paths must not be the same");

        switch (mode) {
        case AccessMode::ReadProtected:
            testtools::copyFileAndRestrictRead(source, destination);
            break;
        case AccessMode::WriteProtected:
            testtools::copyFileAndRestrictWrite(source, destination);
            break;
        }
    }

    ~ScopedTestFile()
    {
        testtools::removeIfExists(m_destinationPath);
    }

    const std::string& getPath() const { return m_destinationPath; }

private:
    std::string m_destinationPath;
};

class Project_Audacity4ProjectTests : public ::testing::Test
{
protected:
    std::unique_ptr<Audacity4Project> m_currentProject = nullptr;
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> m_trackeditProjectCreator;
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> m_projectViewStateCreator;
    std::shared_ptr<au::trackedit::ClipboardMock> m_clipboard;

    void SetUp() override
    {
        m_currentProject = std::make_unique<Audacity4Project>();
        m_currentProject->trackeditProjectCreator.set(m_trackeditProjectCreator);
        m_currentProject->viewStateCreator.set(m_projectViewStateCreator);
        m_currentProject->clipboard.set(m_clipboard);
    }

    void TearDown() override
    {
        if (m_currentProject) {
            // can't close project in all tests, please see individual tests:
            // m_currentProject->close();
            m_currentProject.reset();
        }
    }
};

TEST_F(Project_Audacity4ProjectTests, Load_ValidFile_ReturnsSuccess)
{
    const muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3";

    EXPECT_EQ(muse::io::FileInfo::exists(testPath), true);

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_FileDoesNotExist_ReturnsProjectFileNotFound)
{
    const muse::io::path_t testPath = "/nonexistent/project.aup3";

    // Ensure the path truly doesn't exist
    ASSERT_FALSE(muse::io::FileInfo::exists(testPath));

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileNotFound));

    const auto title = ret.data<std::string>("title", std::string(""));
    const auto body  = ret.data<std::string>("body", std::string(""));

    ASSERT_TRUE(!title.empty());
    ASSERT_TRUE(!body.empty());
    EXPECT_THAT(title, ::testing::HasSubstr("Cannot read file"));
    //can't close m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_FileCannotBeOpened_ReturnsReadProtected)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_read_protected.aup3").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::ReadProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsReadProtected));
    EXPECT_TRUE(!ret.data<std::string>("body", std::string("")).empty());
    //can't close m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_EmptyFileIsWriteProtected_ReturnsSuccess)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_write_protected.aup3").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::WriteProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_NonEmptyFileIsWriteProtected_ReturnsWriteProtected)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test.aup3").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test_write_protected.aup3").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::WriteProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::ProjectFileIsWriteProtected));
    EXPECT_TRUE(!ret.data<std::string>("body", std::string("")).empty());
    m_currentProject->close();
}
} // namespace au::project
