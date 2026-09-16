/*
 * Audacity: A Digital Audio Editor
 */

#include <gtest/gtest.h>

#include "../internal/exportfilename.h"

using namespace au::importexport;

TEST(ExportFilename, UnsavedImportUsesSourceName)
{
    const auto name = defaultExportFilename("New Project 2026-09-15 N-1.aup4unsaved", "My recording");
    EXPECT_EQ(name, QString("My recording"));
    EXPECT_EQ(defaultExportFilename("", "My recording"), QString("My recording"));
    EXPECT_EQ(exportFilenameWithExtension(name, "mp3", true), QString("My recording.mp3"));
    EXPECT_EQ(exportFilenameWithExtension(name, "ogg", true), QString("My recording.ogg"));
}

TEST(ExportFilename, RecordingWithoutSourceUsesUntitled)
{
    EXPECT_EQ(defaultExportFilename("New Project.aup4unsaved", ""), QString("Untitled"));
    EXPECT_EQ(defaultExportFilename("", ""), QString("Untitled"));
}

TEST(ExportFilename, SavedProjectKeepsItsName)
{
    EXPECT_EQ(defaultExportFilename("Edited interview", "Original recording"), QString("Edited interview"));
}

TEST(ExportFilename, GeneratedStemKeepsDotsAndUnicode)
{
    const auto name = defaultExportFilename("New Project.aup4unsaved", QString::fromUtf8("Caf\xc3\xa9.take.2"));
    EXPECT_EQ(exportFilenameWithExtension(name, "wav", true), QString::fromUtf8("Caf\xc3\xa9.take.2.wav"));
    EXPECT_EQ(exportFilenameWithExtension(name, "ogg", true), QString::fromUtf8("Caf\xc3\xa9.take.2.ogg"));
}

TEST(ExportFilename, GeneratedStemMayEndWithAnAudioExtension)
{
    EXPECT_EQ(exportFilenameWithExtension("Archive.mp3", "ogg", true), QString("Archive.mp3.ogg"));
}

TEST(ExportFilename, ExplicitFilenameIsPreserved)
{
    EXPECT_EQ(exportFilenameWithExtension("My custom name.MP3", "mp3", false), QString("My custom name.MP3"));
    EXPECT_EQ(exportFilenameWithExtension("My custom name", "mp3", false), QString("My custom name.mp3"));
    EXPECT_EQ(exportFilenameWithExtension("take.custom", "wav", false), QString("take.custom"));
}

TEST(ExportFilename, EmptyFormatExtensionDoesNotAddADot)
{
    EXPECT_EQ(exportFilenameWithExtension("My recording", "", true), QString("My recording"));
}
