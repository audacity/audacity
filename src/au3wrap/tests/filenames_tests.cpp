/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <filesystem>

#include "au3-files/FileNames.h"

namespace fs = std::filesystem;

class FileNamesTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_base = fs::temp_directory_path() / "au3_filenames_tests";
        fs::remove_all(m_base);
        fs::create_directories(m_base / "real");
        fs::create_directories(m_base / "other");
    }

    void TearDown() override
    {
        fs::remove_all(m_base);
    }

    wxString path(const char* name) const
    {
        return wxString::FromUTF8((m_base / name).string());
    }

    fs::path m_base;
};

TEST_F(FileNamesTests, ExactDuplicatesAreRemovedKeepingTheFirst)
{
    FilePaths paths { path("real"), path("other"), path("real") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    ASSERT_EQ(paths.size(), 2);
    EXPECT_EQ(paths[0], path("real"));
    EXPECT_EQ(paths[1], path("other"));
}

TEST_F(FileNamesTests, DistinctDirectoriesAreKept)
{
    FilePaths paths { path("real"), path("other") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    EXPECT_EQ(paths.size(), 2);
}

TEST_F(FileNamesTests, TrailingSlashSpellingOfSameDirectoryIsRemoved)
{
    FilePaths paths { path("real"), path("real") + "/" };
    FileNames::RemoveDuplicatesFromPathList(paths);
    ASSERT_EQ(paths.size(), 1);
    EXPECT_EQ(paths[0], path("real"));
}

TEST_F(FileNamesTests, DotDotSpellingOfSameDirectoryIsRemoved)
{
    FilePaths paths { path("real"), path("other/../real") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    ASSERT_EQ(paths.size(), 1);
    EXPECT_EQ(paths[0], path("real"));
}

#ifndef _WIN32
TEST_F(FileNamesTests, SymlinkToAlreadyListedDirectoryIsRemoved)
{
    fs::create_directory_symlink(m_base / "real", m_base / "link");
    FilePaths paths { path("real"), path("link") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    ASSERT_EQ(paths.size(), 1);
    EXPECT_EQ(paths[0], path("real"));
}
#endif

TEST_F(FileNamesTests, NonexistentDuplicateSpellingsAreRemoved)
{
    FilePaths paths { path("no-such-dir"), path("no-such-dir") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    EXPECT_EQ(paths.size(), 1);
}

TEST_F(FileNamesTests, NonexistentDirectoryIsNotMergedWithExistingOne)
{
    FilePaths paths { path("no-such-dir"), path("real") };
    FileNames::RemoveDuplicatesFromPathList(paths);
    EXPECT_EQ(paths.size(), 2);
}
