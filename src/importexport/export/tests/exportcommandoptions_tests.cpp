/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "importexport/export/internal/exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;

namespace {
const OutputPathContext SAVED_PROJECT { io::path_t("/out/macro-output"), io::path_t("/music"), "song" };
const OutputPathContext UNSAVED_PROJECT { io::path_t("/out/macro-output"), std::nullopt, "song" };
}

TEST(ExportCommandOptionsTests, AudioRejectsAMissingFormat)
{
    EXPECT_EQ(exportFormatSettings(ExportCommandKind::Audio, {}).ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, SampleRateDefaultsToTheProjectRate)
{
    EXPECT_EQ(exportSampleRate({}, 48000).val, 48000);
    EXPECT_EQ(exportSampleRate({ { "sampleRate", Val(22050) } }, 48000).val, 22050);
    EXPECT_EQ(exportSampleRate({ { "sampleRate", Val(0) } }, 48000).ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, ChannelsDefaultToStereo)
{
    EXPECT_EQ(exportChannelLayout({}).val, ExportChannelLayout::Stereo);
    EXPECT_EQ(exportChannelLayout({ { "channels", Val("Mono") } }).val, ExportChannelLayout::Mono);
    EXPECT_EQ(exportChannelLayout({ { "channels", Val("Surround") } }).ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathDefaultsToTheMacroOutputFolder)
{
    const RetVal<io::path_t> path = exportOutputPath({}, SAVED_PROJECT, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/out/macro-output/song.wav"));
}

TEST(ExportCommandOptionsTests, PathReplacesTheVariablesAndAddsTheExtension)
{
    const RetVal<io::path_t> path = exportOutputPath({ { "path", Val("{ProjectDir}/{ProjectName}-hq") } }, SAVED_PROJECT, "mp3");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/music/song-hq.mp3"));
}

TEST(ExportCommandOptionsTests, PathKeepsAnExistingExtension)
{
    const RetVal<io::path_t> path = exportOutputPath({ { "path", Val("/tmp/{ProjectName}.flac") } }, SAVED_PROJECT, "flac");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/tmp/song.flac"));
}

TEST(ExportCommandOptionsTests, PathRejectsProjectDirForAnUnsavedProject)
{
    const RetVal<io::path_t> path = exportOutputPath({ { "path", Val("{ProjectDir}/out") } }, UNSAVED_PROJECT, "ogg");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathRejectsARelativePath)
{
    const RetVal<io::path_t> path = exportOutputPath({ { "path", Val("out/{ProjectName}") } }, SAVED_PROJECT, "wav");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}
