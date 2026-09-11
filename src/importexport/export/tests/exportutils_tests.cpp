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
    EXPECT_EQ(utils::separateFileName("", 3, "Vocals"), "03.Vocals");
}

TEST(ExportUtilsTests, SeparateFileNamePutsPrefixFirst)
{
    EXPECT_EQ(utils::separateFileName("Song", std::nullopt, "Vocals"), "Song.Vocals");
    EXPECT_EQ(utils::separateFileName("Song", 12, "Vocals"), "Song.12.Vocals");
}

TEST(ExportUtilsTests, SeparateFileNameSkipsEmptyParts)
{
    EXPECT_EQ(utils::separateFileName("", 1, ""), "01");
    EXPECT_EQ(utils::separateFileName("Song", std::nullopt, ""), "Song");
}

TEST(ExportUtilsTests, UniqueFileNamesKeepFirstOccurrence)
{
    utils::UniqueFileNames names;
    EXPECT_EQ(names.registerName("Vocals"), "Vocals");
}

TEST(ExportUtilsTests, UniqueFileNamesNumberDuplicates)
{
    utils::UniqueFileNames names;
    names.registerName("Vocals");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-2");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-3");
}

TEST(ExportUtilsTests, UniqueFileNamesIgnoreCase)
{
    utils::UniqueFileNames names;
    names.registerName("Vocals");
    EXPECT_EQ(names.registerName("VOCALS"), "VOCALS-2");
}

TEST(ExportUtilsTests, UniqueFileNamesSkipTakenSuffixes)
{
    utils::UniqueFileNames names;
    names.registerName("Vocals");
    names.registerName("Vocals-2");
    EXPECT_EQ(names.registerName("Vocals"), "Vocals-3");
}

TEST(ExportUtilsTests, LabelExportRangesKeepRegionLabels)
{
    const std::vector<utils::TimeRange> ranges = utils::labelExportRanges({ { 1.0, 2.5 }, { 3.0, 4.0 } }, 10.0);

    ASSERT_EQ(ranges.size(), 2u);
    EXPECT_DOUBLE_EQ(ranges[0].start, 1.0);
    EXPECT_DOUBLE_EQ(ranges[0].end, 2.5);
    EXPECT_DOUBLE_EQ(ranges[1].start, 3.0);
    EXPECT_DOUBLE_EQ(ranges[1].end, 4.0);
}

TEST(ExportUtilsTests, LabelExportRangesExtendPointLabelsToNextLabel)
{
    const std::vector<utils::TimeRange> ranges = utils::labelExportRanges({ { 1.0, 1.0 }, { 3.0, 3.0 }, { 6.0, 7.0 } }, 10.0);

    ASSERT_EQ(ranges.size(), 3u);
    EXPECT_DOUBLE_EQ(ranges[0].end, 3.0);
    EXPECT_DOUBLE_EQ(ranges[1].end, 6.0);
    EXPECT_DOUBLE_EQ(ranges[2].end, 7.0);
}

TEST(ExportUtilsTests, LabelExportRangesExtendLastPointLabelToProjectEnd)
{
    const std::vector<utils::TimeRange> ranges = utils::labelExportRanges({ { 4.0, 4.0 } }, 10.0);

    ASSERT_EQ(ranges.size(), 1u);
    EXPECT_DOUBLE_EQ(ranges[0].start, 4.0);
    EXPECT_DOUBLE_EQ(ranges[0].end, 10.0);
}
