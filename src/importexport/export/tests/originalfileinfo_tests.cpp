#include <gtest/gtest.h>

#include <memory>
#include <vector>

#include "au3-project/Project.h"
#include "importexport/export/OriginalFileInfo.h"

namespace {
struct FormatCodecCase
{
    const char* extension;
    const char* formatID;
    int sampleRate;
    int channels;
    int bitDepth;
    int bitRate;
};

AudacityProject& freshProject()
{
    static std::vector<std::shared_ptr<AudacityProject> > projects;
    projects.push_back(AudacityProject::Create());
    return *projects.back();
}

au::importexport::ExportParameters makeExportParameters()
{
    return {
        { 1, 256000 },
        { 2, 24 },
        { 3, std::string("joint-stereo") }
    };
}
}

class OriginalFileInfoFormatCodecTests : public ::testing::TestWithParam<FormatCodecCase>
{
};

TEST(OriginalFileInfoTests, InitiallyHasNoOriginalFile)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_TRUE(info.GetOriginalFilePath().isEmpty());
    EXPECT_TRUE(info.GetOriginalFileName().isEmpty());
    EXPECT_TRUE(info.GetExportFormatID().isEmpty());
    EXPECT_TRUE(info.GetCodecSettings().isEmpty());
    EXPECT_TRUE(info.GetExportParameters().empty());
    EXPECT_EQ(info.GetImportedFileCount(), 0);
}

TEST(OriginalFileInfoTests, RemembersImportedFilePathFormatAndCodecSettings)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 48000);
    codecSettings.insert("channels", 2);
    codecSettings.insert("bitRate", 256000);
    codecSettings.insert("bitDepth", 24);

    const auto exportParameters = makeExportParameters();

    info.SetOriginalFile("C:/audio/original.wav", "original.wav");
    info.SetExportFormatID("WAV");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(exportParameters);
    info.IncrementImportedFileCount();

    EXPECT_TRUE(info.HasOriginalFile());
    EXPECT_EQ(info.GetOriginalFilePath(), "C:/audio/original.wav");
    EXPECT_EQ(info.GetOriginalFileName(), "original.wav");
    EXPECT_EQ(info.GetExportFormatID(), "WAV");
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), 48000);
    EXPECT_EQ(info.GetCodecSettings().value("channels").toInt(), 2);
    EXPECT_EQ(info.GetCodecSettings().value("bitRate").toInt(), 256000);
    EXPECT_EQ(info.GetCodecSettings().value("bitDepth").toInt(), 24);
    EXPECT_EQ(info.GetExportParameters(), exportParameters);
}

TEST_P(OriginalFileInfoFormatCodecTests, RemembersFormatBitRateBitDepthAndChannelLayout)
{
    const auto testCase = GetParam();
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", testCase.sampleRate);
    codecSettings.insert("channels", testCase.channels);
    codecSettings.insert("bitRate", testCase.bitRate);
    codecSettings.insert("bitDepth", testCase.bitDepth);

    const QString filename = QString("original.") + testCase.extension;
    info.SetOriginalFile(QString("C:/audio/") + filename, filename);
    info.SetExportFormatID(QString(testCase.formatID));
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());
    info.IncrementImportedFileCount();

    EXPECT_TRUE(info.HasOriginalFile());
    EXPECT_EQ(info.GetOriginalFileName(), filename);
    EXPECT_EQ(info.GetExportFormatID(), QString(testCase.formatID));
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), testCase.sampleRate);
    EXPECT_EQ(info.GetCodecSettings().value("channels").toInt(), testCase.channels);
    EXPECT_EQ(info.GetCodecSettings().value("bitRate").toInt(), testCase.bitRate);
    EXPECT_EQ(info.GetCodecSettings().value("bitDepth").toInt(), testCase.bitDepth);
    EXPECT_FALSE(info.GetExportParameters().empty());
}

INSTANTIATE_TEST_SUITE_P(
    OriginalFileInfoTests,
    OriginalFileInfoFormatCodecTests,
    ::testing::Values(
        FormatCodecCase { "wav", "WAV", 44100, 1, 16, 705600 },
        FormatCodecCase { "wav", "WAV", 48000, 2, 24, 2304000 },
        FormatCodecCase { "flac", "FLAC", 96000, 1, 24, 900000 },
        FormatCodecCase { "flac", "FLAC", 44100, 2, 16, 650000 },
        FormatCodecCase { "mp3", "MP3", 44100, 1, 0, 128000 },
        FormatCodecCase { "mp3", "MP3", 48000, 2, 0, 229000 },
        FormatCodecCase { "ogg", "OGG", 44100, 1, 0, 96000 },
        FormatCodecCase { "ogg", "OGG", 48000, 2, 0, 160000 }));

TEST(OriginalFileInfoTests, MultipleImportsKeepMetadataButDisableOverwriteOriginal)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 96000);
    codecSettings.insert("bitDepth", 24);

    info.SetOriginalFile("C:/audio/first.flac", "first.flac");
    info.SetExportFormatID("FLAC");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());

    info.IncrementImportedFileCount();
    info.IncrementImportedFileCount();

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_EQ(info.GetImportedFileCount(), 2);
    EXPECT_EQ(info.GetOriginalFilePath(), "C:/audio/first.flac");
    EXPECT_EQ(info.GetExportFormatID(), "FLAC");
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), 96000);
}

TEST(OriginalFileInfoTests, ClearDropsRememberedOverwriteState)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 44100);
    codecSettings.insert("quality", 7);

    info.SetOriginalFile("C:/audio/original.ogg", "original.ogg");
    info.SetExportFormatID("OGG");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());
    info.IncrementImportedFileCount();

    info.Clear();

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_TRUE(info.GetOriginalFilePath().isEmpty());
    EXPECT_TRUE(info.GetOriginalFileName().isEmpty());
    EXPECT_TRUE(info.GetExportFormatID().isEmpty());
    EXPECT_TRUE(info.GetCodecSettings().isEmpty());
    EXPECT_TRUE(info.GetExportParameters().empty());
    EXPECT_EQ(info.GetImportedFileCount(), 0);
}

TEST(OriginalFileInfoTests, StateIsScopedPerProject)
{
    AudacityProject& firstProject = freshProject();
    AudacityProject& secondProject = freshProject();

    auto& firstInfo = OriginalFileInfo::Get(firstProject);
    auto& secondInfo = OriginalFileInfo::Get(secondProject);

    QVariantMap firstSettings;
    firstSettings.insert("bitRate", 192000);

    QVariantMap secondSettings;
    secondSettings.insert("sampleRate", 44100);
    secondSettings.insert("bitDepth", 16);

    firstInfo.SetOriginalFile("C:/audio/first.mp3", "first.mp3");
    firstInfo.SetExportFormatID("MP3");
    firstInfo.SetCodecSettings(firstSettings);
    firstInfo.IncrementImportedFileCount();

    secondInfo.SetOriginalFile("C:/audio/second.wav", "second.wav");
    secondInfo.SetExportFormatID("WAV");
    secondInfo.SetCodecSettings(secondSettings);
    secondInfo.IncrementImportedFileCount();

    EXPECT_EQ(firstInfo.GetOriginalFilePath(), "C:/audio/first.mp3");
    EXPECT_EQ(firstInfo.GetExportFormatID(), "MP3");
    EXPECT_EQ(firstInfo.GetCodecSettings().value("bitRate").toInt(), 192000);

    EXPECT_EQ(secondInfo.GetOriginalFilePath(), "C:/audio/second.wav");
    EXPECT_EQ(secondInfo.GetExportFormatID(), "WAV");
    EXPECT_EQ(secondInfo.GetCodecSettings().value("sampleRate").toInt(), 44100);
    EXPECT_EQ(secondInfo.GetCodecSettings().value("bitDepth").toInt(), 16);
}
