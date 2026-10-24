/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include "../internal/exportfilename.h"

using namespace au::importexport;

TEST(ExportFilename, ImportedTitle)
{
    EXPECT_EQ(defaultExportFilename("session.aup4unsaved", "My recording"), "My recording");
}
TEST(ExportFilename, SavedProject)
{
    EXPECT_EQ(defaultExportFilename("Saved project", "Imported audio"), "Saved project");
}
TEST(ExportFilename, UnnamedRecording)
{
    EXPECT_EQ(defaultExportFilename("session.aup4unsaved", ""), "Untitled");
    EXPECT_EQ(defaultExportFilename("", ""), "Untitled");
}
TEST(ExportFilename, EmptyDisplayName)
{
    EXPECT_EQ(defaultExportFilename("", "Imported audio"), "Imported audio");
}
TEST(ExportFilename, UnicodeAndDots)
{
    const auto stem = QString::fromUtf8("Caf\xc3\xa9.take.2");
    EXPECT_EQ(exportFilenameWithExtensions(stem, { "mp3" }, true), stem + ".mp3");
}
TEST(ExportFilename, StemEndingInFormatExtension)
{
    EXPECT_EQ(exportFilenameWithExtensions("recording.mp3", { "mp3" }, true), "recording.mp3.mp3");
}
TEST(ExportFilename, FormatChanges)
{
    EXPECT_EQ(exportFilenameWithExtensions("My recording", { "mp3" }, true), "My recording.mp3");
    EXPECT_EQ(exportFilenameWithExtensions("My recording", { "ogg" }, true), "My recording.ogg");
}
TEST(ExportFilename, ManualNameWithRecognizedExtension)
{
    EXPECT_EQ(exportFilenameWithExtensions("Custom.MP3", { "mp3" }, false), "Custom.MP3");
    EXPECT_EQ(exportFilenameWithExtensions("Custom.wave", { "wav", "wave" }, false), "Custom.wave");
}
TEST(ExportFilename, ManualNameWithDifferentExtension)
{
    EXPECT_EQ(exportFilenameWithExtensions("Custom.wav", { "mp3" }, false), "Custom.wav.mp3");
}
TEST(ExportFilename, ManualDottedName)
{
    EXPECT_EQ(exportFilenameWithExtensions("Custom.take.2", { "ogg" }, false), "Custom.take.2.ogg");
}
TEST(ExportFilename, NoFormatExtension)
{
    EXPECT_EQ(exportFilenameWithExtensions("Custom.take.2", {}, true), "Custom.take.2");
}
