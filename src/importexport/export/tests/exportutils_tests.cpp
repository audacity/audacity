/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "importexport/export/exportutils.h"

using namespace au::importexport;

TEST(ExportUtilsTests, SeparateFileNameUsesTrackNameAlone)
{
    EXPECT_EQ(utils::separateFileName("", std::nullopt, "Vocals"), "Vocals");
}

TEST(ExportUtilsTests, SeparateFileNamePutsNumberBeforeName)
{
    EXPECT_EQ(utils::separateFileName("", 3, "Vocals"), "03-Vocals");
}

TEST(ExportUtilsTests, SeparateFileNamePutsPrefixFirst)
{
    EXPECT_EQ(utils::separateFileName("Song", std::nullopt, "Vocals"), "Song-Vocals");
    EXPECT_EQ(utils::separateFileName("Song", 12, "Vocals"), "Song-12-Vocals");
}

TEST(ExportUtilsTests, SeparateFileNameSkipsEmptyParts)
{
    EXPECT_EQ(utils::separateFileName("", 1, ""), "01");
    EXPECT_EQ(utils::separateFileName("Song", std::nullopt, ""), "Song");
}

TEST(ExportUtilsTests, MakeFileNameUniqueKeepsFirstOccurrence)
{
    std::vector<std::string> used;
    EXPECT_EQ(utils::makeFileNameUnique("Vocals", used), "Vocals");
    EXPECT_EQ(used, std::vector<std::string> { "Vocals" });
}

TEST(ExportUtilsTests, MakeFileNameUniqueNumbersDuplicates)
{
    std::vector<std::string> used;
    utils::makeFileNameUnique("Vocals", used);
    EXPECT_EQ(utils::makeFileNameUnique("Vocals", used), "Vocals-2");
    EXPECT_EQ(utils::makeFileNameUnique("Vocals", used), "Vocals-3");
}

TEST(ExportUtilsTests, MakeFileNameUniqueIgnoresCase)
{
    std::vector<std::string> used;
    utils::makeFileNameUnique("Vocals", used);
    EXPECT_EQ(utils::makeFileNameUnique("VOCALS", used), "VOCALS-2");
}
