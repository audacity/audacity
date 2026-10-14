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

#include "sqlite3.h"

#include "global/io/fileinfo.h"
#include "global/types/ret.h"
#include "project/projecterrors.h"
#include "project/internal/audacityproject.h"
#include "au3wrap/internal/projectvideoref.h"

#include "au3-project-file-io/ProjectFileIO.h"

#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"
#include "project/tests/mocks/cloudprojectsprovidermock.h"
#include "trackedit/tests/mocks/clipboardmock.h"

#include "testtools.h"
#include "testing/testcontext.h"

namespace au::project {
enum class AccessMode
{
    Full,
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
        case AccessMode::Full:
            testtools::copyFile(source, destination);
            break;
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
        testtools::removeProjectIfExists(m_destinationPath);
    }

    const std::string& getPath() const { return m_destinationPath; }

private:
    std::string m_destinationPath;
};

class Project_Audacity4ProjectTests : public ::testing::Test
{
protected:
    muse::modularity::ContextPtr m_testCtx;
    std::unique_ptr<Audacity4Project> m_currentProject = nullptr;
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> m_trackeditProjectCreator;
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> m_projectViewStateCreator;
    std::shared_ptr<au::trackedit::ClipboardMock> m_clipboard;
    std::shared_ptr<au::au3cloud::CloudProjectsProviderMock> m_cloudProjectsProvider;

    void SetUp() override
    {
        m_testCtx = au::testutils::makeTestContext();

        m_clipboard = std::make_shared<::testing::NiceMock<au::trackedit::ClipboardMock> >();

        m_cloudProjectsProvider = std::make_shared<::testing::NiceMock<au::au3cloud::CloudProjectsProviderMock> >();

        m_currentProject = std::make_unique<Audacity4Project>(m_testCtx);
        m_currentProject->trackeditProjectCreator.set(m_trackeditProjectCreator);
        m_currentProject->viewStateCreator.set(m_projectViewStateCreator);
        m_currentProject->clipboard.set(m_clipboard);
        m_currentProject->cloudProjectsProvider.set(m_cloudProjectsProvider);
    }

    void TearDown() override
    {
        if (m_currentProject) {
            // can't close project in all tests, please see individual tests:
            // m_currentProject->close();
            m_currentProject.reset();
        }
    }

    //! Writes the project to disk through au3's own save, which runs the same
    //! XML writers as a real save. The AU4 wrapper's save() is not usable
    //! here: it needs a real trackedit project and these mocks return null.
    void saveThroughAu3(const muse::io::path_t& path)
    {
        auto* au3Project = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
        ASSERT_NE(au3Project, nullptr);

        auto& fileIO = ProjectFileIO::Get(*au3Project);
        ASSERT_TRUE(fileIO.SaveProject(wxString::FromUTF8(path.toStdString()), nullptr));
    }

    //! Replaces m_currentProject with a freshly wired one. Tests that reopen a
    //! project from disk need this; the injected members are private and a
    //! TEST_F body lives in a derived class, so it cannot do the wiring itself.
    void recreateProject()
    {
        m_currentProject = std::make_unique<Audacity4Project>(m_testCtx);
        m_currentProject->trackeditProjectCreator.set(m_trackeditProjectCreator);
        m_currentProject->viewStateCreator.set(m_projectViewStateCreator);
        m_currentProject->clipboard.set(m_clipboard);
        m_currentProject->cloudProjectsProvider.set(m_cloudProjectsProvider);
    }
};

TEST_F(Project_Audacity4ProjectTests, Load_ValidFile_ReturnsSuccess)
{
    const muse::io::path_t testPath = muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4";

    EXPECT_EQ(muse::io::FileInfo::exists(testPath), true);

    const muse::Ret ret = m_currentProject->load(testPath, false, "");

    EXPECT_TRUE(ret.success());
    EXPECT_EQ(ret.code(), static_cast<int>(Err::NoError));
    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_LegacySchema_UpgradesInPlace)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/legacy_schema.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/legacy_schema_copy.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::Full };

    const auto readSchemaState = [&tempFile]() {
        sqlite3* db = nullptr;
        EXPECT_EQ(sqlite3_open(tempFile.getPath().c_str(), &db), SQLITE_OK);

        int userVersion = 0;
        sqlite3_stmt* stmt = nullptr;
        EXPECT_EQ(sqlite3_prepare_v2(db, "PRAGMA user_version;", -1, &stmt, nullptr), SQLITE_OK);
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            userVersion = sqlite3_column_int(stmt, 0);
        }
        sqlite3_finalize(stmt);

        int historyTables = 0;
        stmt = nullptr;
        EXPECT_EQ(sqlite3_prepare_v2(
                      db, "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='project_history';",
                      -1, &stmt, nullptr), SQLITE_OK);
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            historyTables = sqlite3_column_int(stmt, 0);
        }
        sqlite3_finalize(stmt);
        sqlite3_close(db);
        return std::make_pair(userVersion, historyTables);
    };

    const auto [versionBefore, historyBefore] = readSchemaState();
    EXPECT_GT(versionBefore, 0);
    EXPECT_EQ(historyBefore, 0);

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");
    EXPECT_TRUE(ret.success());
    m_currentProject->close();

    const auto [versionAfter, historyAfter] = readSchemaState();
    EXPECT_GT(versionAfter, versionBefore);
    EXPECT_EQ(historyAfter, 1);
}

TEST_F(Project_Audacity4ProjectTests, Load_FileDoesNotExist_ReturnsProjectFileNotFound)
{
    const muse::io::path_t testPath = "/nonexistent/project.aup4";

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

TEST_F(Project_Audacity4ProjectTests, Load_FileCannotBeOpened_ReturnsCantOpen)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_read_protected.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::ReadProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), SQLITE_CANTOPEN);
    EXPECT_TRUE(!ret.data<std::string>("body", std::string("")).empty());
    //can't close m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_EmptyFileIsWriteProtected_ReturnsReadOnly)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty_write_protected.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::WriteProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), SQLITE_READONLY);
    EXPECT_TRUE(!ret.data<std::string>("body", std::string("")).empty());
    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, Load_NonEmptyFileIsWriteProtected_ReturnsReadOnly)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/test_write_protected.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::WriteProtected };

    const muse::Ret ret = m_currentProject->load(muse::io::path_t(tempFile.getPath()), false, "");

    EXPECT_FALSE(ret.success());
    EXPECT_EQ(ret.code(), SQLITE_READONLY);
    EXPECT_TRUE(!ret.data<std::string>("body", std::string("")).empty());
    m_currentProject->close();
}

// ---------------------------------------------------------------------------
// An attached video is recorded against the project, so it comes back when
// the project is reopened. It is stored per project rather than against a
// track or a clip because clip identifiers are never written to the project
// file and are reassigned by split, delete and paste.
// ---------------------------------------------------------------------------

TEST_F(Project_Audacity4ProjectTests, VideoReference_SurvivesSaveAndReload)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/video_roundtrip.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::Full };
    const muse::io::path_t path{ tempFile.getPath() };

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());

    {
        auto* au3Project = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
        ASSERT_NE(au3Project, nullptr);

        auto& ref = au::au3::ProjectVideoRef::Get(*au3Project);
        EXPECT_TRUE(ref.isEmpty()) << "a project with no video should start empty";

        ref.setPath("/media/library/interview.mkv");
        ref.setRelativePath("../media/interview.mkv");
        ref.setDuration(123.456);
        ref.setFrameRate(25.0);

        // Negative on purpose: the sign is the part most likely to be lost by
        // a format that writes the number as text.
        ref.setOffset(-1.25);
    }

    saveThroughAu3(path);
    m_currentProject->close();

    // Reopen from disk. Everything below has been through XML and back.
    recreateProject();

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());

    auto* reopened = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
    ASSERT_NE(reopened, nullptr);

    const auto& ref = au::au3::ProjectVideoRef::Get(*reopened);

    EXPECT_FALSE(ref.isEmpty());
    EXPECT_EQ(ref.path(), "/media/library/interview.mkv");
    EXPECT_EQ(ref.relativePath(), "../media/interview.mkv");

    // Duration and frame rate come back too: they are what lets a path that
    // still resolves after the media was replaced be noticed.
    EXPECT_NEAR(ref.duration(), 123.456, 1e-6);
    EXPECT_NEAR(ref.frameRate(), 25.0, 1e-9);

    // The offset is the only value here the user sets by hand, so losing it
    // silently would mean re-aligning the video after every reopen.
    EXPECT_NEAR(ref.offset(), -1.25, 1e-6);

    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, VideoReference_AbsentWhenNoVideoWasAttached)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/video_absent.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::Full };
    const muse::io::path_t path{ tempFile.getPath() };

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());
    saveThroughAu3(path);
    m_currentProject->close();

    recreateProject();

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());

    auto* reopened = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
    ASSERT_NE(reopened, nullptr);

    // Nothing is written when there is no video, so a project that never had
    // one stays byte-identical in this respect.
    EXPECT_TRUE(au::au3::ProjectVideoRef::Get(*reopened).isEmpty());

    m_currentProject->close();
}

TEST_F(Project_Audacity4ProjectTests, VideoReference_DetachClearsItFromTheProject)
{
    const std::string srcPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4").toStdString();
    const std::string dstPath = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/video_cleared.aup4").toStdString();

    const ScopedTestFile tempFile{ srcPath, dstPath, AccessMode::Full };
    const muse::io::path_t path{ tempFile.getPath() };

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());
    {
        auto* au3Project = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
        auto& ref = au::au3::ProjectVideoRef::Get(*au3Project);
        ref.setPath("/media/library/interview.mkv");
        ref.setDuration(10.0);
    }
    saveThroughAu3(path);

    // Detaching clears the record; saving again must not leave the old path.
    {
        auto* au3Project = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
        au::au3::ProjectVideoRef::Get(*au3Project).clear();
    }
    saveThroughAu3(path);
    m_currentProject->close();

    recreateProject();

    ASSERT_TRUE(m_currentProject->load(path, false, "").success());

    auto* reopened = reinterpret_cast<AudacityProject*>(m_currentProject->au3ProjectPtr());
    EXPECT_TRUE(au::au3::ProjectVideoRef::Get(*reopened).isEmpty())
        << "a detached video must not come back";

    m_currentProject->close();
}
} // namespace au::project
