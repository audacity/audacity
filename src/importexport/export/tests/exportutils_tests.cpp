/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "importexport/export/exportutils.h"

using namespace au::importexport;

TEST(ExportUtilsTests, FormatFileNameUsesTrackNameAlone)
{
    EXPECT_EQ(utils::formatFileName("", std::nullopt, "Vocals"), "Vocals");
}

TEST(ExportUtilsTests, FormatFileNamePutsNumberBeforeName)
{
    EXPECT_EQ(utils::formatFileName("", 3, "Vocals"), "03.Vocals");
}

TEST(ExportUtilsTests, FormatFileNamePutsPrefixFirst)
{
    EXPECT_EQ(utils::formatFileName("Song", std::nullopt, "Vocals"), "Song.Vocals");
    EXPECT_EQ(utils::formatFileName("Song", 12, "Vocals"), "Song.12.Vocals");
}

TEST(ExportUtilsTests, FormatFileNameSkipsEmptyParts)
{
    EXPECT_EQ(utils::formatFileName("", 1, ""), "01");
    EXPECT_EQ(utils::formatFileName("Song", std::nullopt, ""), "Song");
}

TEST(ExportUtilsTests, SanitizeFileNameReplacesInvalidCharacters)
{
    EXPECT_EQ(utils::sanitizeFileName("a/b\\c:d*e?f\"g<h>i|j~k"), "a_b_c_d_e_f_g_h_i_j_k");
    EXPECT_EQ(utils::sanitizeFileName("tab\there"), "tab_here");
}

TEST(ExportUtilsTests, SanitizeFileNameNormalizesWindowsReservedDeviceNames)
{
    EXPECT_EQ(utils::sanitizeFileName("CON"), "CON_");
    EXPECT_EQ(utils::sanitizeFileName("nul"), "nul_");
    EXPECT_EQ(utils::sanitizeFileName("COM1.Vocals"), "COM1_.Vocals");
    EXPECT_EQ(utils::sanitizeFileName("LPT\u00b9"), "LPT\u00b9_");
    EXPECT_EQ(utils::sanitizeFileName("Song.CON"), "Song.CON");
    EXPECT_EQ(utils::sanitizeFileName("CONSOLE"), "CONSOLE");
}

TEST(ExportUtilsTests, SanitizeFileNameKeepsUnicodeAndPunctuation)
{
    EXPECT_EQ(utils::sanitizeFileName("Ärger & Éclat's (take 2).wav"), "Ärger & Éclat's (take 2).wav");
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

TEST(ExportUtilsTests, UniqueFileNamesIgnoreUnicodeCase)
{
    utils::UniqueFileNames names;
    names.registerName("Ä");
    EXPECT_EQ(names.registerName("ä"), "ä-2");
    names.registerName("Ünïcødé");
    EXPECT_EQ(names.registerName("ünïcødé"), "ünïcødé-2");
}

TEST(ExportUtilsTests, UniqueFileNamesTreatCanonicallyEquivalentNamesAsDuplicates)
{
    utils::UniqueFileNames names;
    names.registerName("\u00e9");
    EXPECT_EQ(names.registerName("e\u0301"), "e\u0301-2");
}

TEST(ExportUtilsTests, MakeFileNameFormatsSanitizesAndDeduplicates)
{
    utils::UniqueFileNames names;
    EXPECT_EQ(utils::makeFileName("Song", 1, "A/B", names), "Song.01.A_B");
    EXPECT_EQ(utils::makeFileName("Song", 1, "a\\b", names), "Song.01.a_b-2");
}

TEST(ExportUtilsTests, MakeFileNameDeduplicatesNormalizedReservedNames)
{
    utils::UniqueFileNames names;
    EXPECT_EQ(utils::makeFileName("", std::nullopt, "CON", names), "CON_");
    EXPECT_EQ(utils::makeFileName("", std::nullopt, "con", names), "con_-2");
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
