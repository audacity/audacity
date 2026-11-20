/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>

#include <QDir>
#include <QFile>
#include <QTemporaryDir>
#include <sqlite3.h>
#include <wx/log.h>

#include "au3-exceptions/AudacityException.h"
#include "au3-files/TempDirectory.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "project/projecterrors.h"
#include "global/settings.h"
#include "testing/testcontext.h"

namespace au::project {
class ProjectRecoveryTests : public ::testing::TestWithParam<bool>
{
protected:
    void SetUp() override
    {
        m_previousTempDir = muse::settings()->value(tempDirKey());
        ASSERT_TRUE(m_directory.isValid());
        const auto projectDirectory = m_directory.path() + QString::fromUtf8("/Тест/SessionData");
        ASSERT_TRUE(QDir().mkpath(projectDirectory));
        m_path = projectDirectory + "/project.aup4unsaved";
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", m_path));

        sqlite3* db = nullptr;
        ASSERT_EQ(sqlite3_open(m_path.toUtf8().constData(), &db), SQLITE_OK);
        const auto sql = GetParam()
                         ? "DELETE FROM autosave; INSERT INTO autosave SELECT * FROM project; DELETE FROM project;"
                         : "DELETE FROM autosave; DELETE FROM project;";
        const int result = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
        sqlite3_close(db);
        ASSERT_EQ(result, SQLITE_OK);
    }

    void TearDown() override
    {
        muse::settings()->setLocalValue(tempDirKey(), m_previousTempDir);
        TempDirectory::ResetTempDir();
    }

    static int64_t countRows(const QString& path, const char* table)
    {
        sqlite3* db = nullptr;
        EXPECT_EQ(sqlite3_open(path.toUtf8().constData(), &db), SQLITE_OK);
        sqlite3_stmt* statement = nullptr;
        const std::string sql = std::string("SELECT count(*) FROM ") + table + ";";
        EXPECT_EQ(sqlite3_prepare_v2(db, sql.c_str(), -1, &statement, nullptr), SQLITE_OK);
        int64_t count = -1;
        if (sqlite3_step(statement) == SQLITE_ROW) {
            count = sqlite3_column_int64(statement, 0);
        }
        sqlite3_finalize(statement);
        sqlite3_close(db);
        return count;
    }

    static void insertSampleBlock(const QString& path)
    {
        sqlite3* db = nullptr;
        ASSERT_EQ(sqlite3_open(path.toUtf8().constData(), &db), SQLITE_OK);
        const int result = sqlite3_exec(db,
                                        "INSERT INTO sampleblocks(sampleformat, summin, summax, sumrms, summary256, summary64k, samples)"
                                        " VALUES(262159, 0, 0, 0, zeroblob(12), zeroblob(12), zeroblob(4096));",
                                        nullptr, nullptr, nullptr);
        sqlite3_close(db);
        ASSERT_EQ(result, SQLITE_OK);
    }

    static muse::Settings::Key tempDirKey()
    {
        return { "au3wrap", "Directories/TempDir" };
    }

    QTemporaryDir m_directory;
    QString m_path;
    muse::Val m_previousTempDir;
};

TEST_P(ProjectRecoveryTests, IgnoringAutosaveKeepsTheLoadedConnection)
{
    const auto context = testutils::makeTestContext();
    au3::Au3ProjectAccessor project(context);
    ASSERT_TRUE(project.load(m_path, true));

    auto& fileIO = ProjectFileIO::Get(*reinterpret_cast<AudacityProject*>(project.au3ProjectPtr()));
    EXPECT_TRUE(fileIO.HasConnection());
    EXPECT_EQ(project.getFileName(), muse::io::path_t(m_path));
    EXPECT_TRUE(project.isTemporary());

    if (fileIO.HasConnection()) {
        project.close();
    }
}

TEST_P(ProjectRecoveryTests, DiscardDoesNotCreateAnotherTemporaryDatabase)
{
    // A file in place of the configured temp directory makes any attempt to
    // open an unrelated temporary database fail, reproducing the crash path.
    const auto unavailableTempDir = m_directory.path() + "/not-a-directory";
    QFile blocker(unavailableTempDir);
    ASSERT_TRUE(blocker.open(QIODevice::WriteOnly));
    blocker.close();
    muse::settings()->setLocalValue(tempDirKey(), muse::Val(unavailableTempDir));
    TempDirectory::ResetTempDir();

    wxLogNull suppressExpectedDirectoryErrors;
    au3::Au3ProjectCreator creator;
    try {
        const auto result = creator.removeUnsavedData(m_path);
        EXPECT_TRUE(result);
    } catch (const MessageBoxException& exception) {
        FAIL() << exception.ErrorMessage().translated().toStdString();
    }
    EXPECT_FALSE(QFile::exists(m_path));
}

TEST_P(ProjectRecoveryTests, AudioWithoutDocumentIsRefused)
{
    //! [GIVEN] A recording session that died before any document was written
    insertSampleBlock(m_path);

    //! [WHEN] It is opened
    const auto context = testutils::makeTestContext();
    au3::Au3ProjectAccessor project(context);
    const muse::Ret ret = project.load(m_path, false);

    if (GetParam()) {
        //! [THEN] With an autosave document it opens as a recovered project
        ASSERT_TRUE(ret);
        project.close();
    } else {
        //! [THEN] Without any document the load is refused instead of opening an empty project
        EXPECT_EQ(ret.code(), int(Err::NoDocument));
    }

    //! [THEN] The audio is untouched either way
    EXPECT_EQ(countRows(m_path, "sampleblocks"), 1);
}

INSTANTIATE_TEST_SUITE_P(WithAndWithoutAutosave, ProjectRecoveryTests, ::testing::Bool());
}
