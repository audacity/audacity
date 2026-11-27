/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/database.h"
#include "projectrecovery/projectrecoveryerrors.h"

#include "testprojectbuilder.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
class DatabaseTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_dir = std::filesystem::temp_directory_path() / "au_projectrecovery_tests"
                / ::testing::UnitTest::GetInstance()->current_test_info()->name();
        std::filesystem::remove_all(m_dir);
        std::filesystem::create_directories(m_dir);
        const auto path = m_dir / "project.aup4";
        TestProject project;
        project.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 1, 1000 } } } } });
        writeProjectFile(path, project);
        ASSERT_TRUE(m_db.open(muse::io::path_t(path.string()), Database::Mode::ReadOnly));
    }

    void TearDown() override
    {
        m_db.close();
        std::filesystem::remove_all(m_dir);
    }

    //! Whether the query ran: it always returns one row when it runs
    bool runs(const std::vector<DatabaseValue>& values, muse::Ret& ret)
    {
        bool rowSeen = false;
        ret = m_db.query("SELECT count(*) FROM sampleblocks WHERE blockid = ?1;", values,
                         [&rowSeen](const DatabaseRow&) { rowSeen = true; });
        return rowSeen;
    }

    std::filesystem::path m_dir;
    Database m_db;
};
}

TEST_F(DatabaseTests, AQueryWithMoreValuesThanParametersFails)
{
    //! [WHEN] A query gets a value for a parameter that it does not have
    muse::Ret ret;
    const bool ran = runs({ int64_t(1), int64_t(2) }, ret);

    //! [THEN] The query fails and does not run
    EXPECT_FALSE(ret);
    EXPECT_FALSE(ran);
}

TEST_F(DatabaseTests, AQueryWithFewerValuesThanParametersFails)
{
    //! [WHEN] A query gets no value for its parameter
    muse::Ret ret;
    const bool ran = runs({}, ret);

    //! [THEN] The query fails and does not run, the parameter is not read as NULL
    EXPECT_FALSE(ret);
    EXPECT_FALSE(ran);
}

TEST_F(DatabaseTests, AQueryWithAValueForEachParameterRuns)
{
    //! [WHEN] A query gets one value for its one parameter
    muse::Ret ret;
    const bool ran = runs({ int64_t(1) }, ret);

    //! [THEN] It runs
    EXPECT_TRUE(ret) << ret.toString();
    EXPECT_TRUE(ran);
}

TEST_F(DatabaseTests, HasTableTellsWhetherATableExists)
{
    //! [WHEN] Tables of the project are looked up
    const muse::RetVal<bool> blocks = m_db.hasTable("sampleblocks");
    const muse::RetVal<bool> other = m_db.hasTable("no_such_table");

    //! [THEN] Both lookups succeed, and only the existing table is found
    ASSERT_TRUE(blocks.ret) << blocks.ret.toString();
    EXPECT_TRUE(blocks.val);
    ASSERT_TRUE(other.ret) << other.ret.toString();
    EXPECT_FALSE(other.val);
}

TEST_F(DatabaseTests, ALookupThatCannotRunIsAnErrorAndNotAnAbsentTable)
{
    //! [GIVEN] A closed database
    m_db.close();

    //! [WHEN] A table and a pragma are looked up
    const muse::RetVal<bool> table = m_db.hasTable("sampleblocks");
    const muse::RetVal<int64_t> pageSize = m_db.pragmaInteger("page_size");

    //! [THEN] Both report an error
    EXPECT_FALSE(table.ret);
    EXPECT_FALSE(pageSize.ret);
}

TEST_F(DatabaseTests, PragmaIntegerReadsTheValue)
{
    //! [WHEN] The page size is read
    const muse::RetVal<int64_t> pageSize = m_db.pragmaInteger("page_size");

    //! [THEN] It is a valid SQLite page size
    ASSERT_TRUE(pageSize.ret) << pageSize.ret.toString();
    EXPECT_GE(pageSize.val, 512);
    EXPECT_LE(pageSize.val, 65536);
}

TEST_F(DatabaseTests, AWriteThatSQLiteRefusesIsAWriteFailure)
{
    //! [WHEN] A read-only connection writes
    const muse::Ret ret = m_db.exec("DELETE FROM sampleblocks;");

    //! [THEN] The error says that writing failed, with SQLite's text as the details
    EXPECT_EQ(ret.code(), static_cast<int>(Err::WriteFailed));
    EXPECT_FALSE(ret.text().empty());
}

#ifndef _WIN32
TEST_F(DatabaseTests, APathThatStartsWithTwoSlashesOpens)
{
    //! [GIVEN] A path with two leading slashes, as a Windows network share path has; here it is the same file
    const std::string path = "/" + (m_dir / "project.aup4").string();

    //! [WHEN] It is opened
    Database database;
    const muse::Ret ret = database.open(muse::io::path_t(path), Database::Mode::Immutable);

    //! [THEN] SQLite does not read the first folder as a host name
    EXPECT_TRUE(ret) << ret.toString();
}
#endif
