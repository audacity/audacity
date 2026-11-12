/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "importexport/export/internal/exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
const OutputPathContext SAVED_PROJECT { io::path_t("/out/macro-output"), io::path_t("/music"), "song" };
const OutputPathContext UNSAVED_PROJECT { io::path_t("/out/macro-output"), std::nullopt, "song" };

template<typename C>
Ret convert(const Params& params, C& command)
{
    return fromInputParams(params, command).ret;
}
}

TEST(ExportCommandOptionsTests, AudioRejectsAMissingFormat)
{
    ExportAudioCommand command;
    EXPECT_EQ(convert({}, command).code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, Mp3RejectsAQualityThatIsNotInTheEnum)
{
    ExportMp3VbrCommand command;
    EXPECT_EQ(convert({ { "quality", Val("V10") } }, command).code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, FlacRejectsAMissingLevel)
{
    ExportFlacCommand command;
    EXPECT_EQ(convert({ { "bitDepth", Val("16") } }, command).code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, SampleRateIsZeroWhenNotGivenAndMustBePositive)
{
    ExportWavCommand notGiven;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") } }, notGiven));
    EXPECT_EQ(notGiven.sampleRate, 0);

    ExportWavCommand given;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") }, { "sampleRate", Val(22050) } }, given));
    EXPECT_EQ(given.sampleRate, 22050);

    ExportWavCommand zero;
    EXPECT_EQ(convert({ { "encoding", Val("PCM16") }, { "sampleRate", Val(0) } }, zero).code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, ChannelsDefaultToStereo)
{
    ExportWavCommand notGiven;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") } }, notGiven));
    EXPECT_EQ(notGiven.channels, ExportChannelLayout::Stereo);

    ExportWavCommand mono;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") }, { "channels", Val("Mono") } }, mono));
    EXPECT_EQ(mono.channels, ExportChannelLayout::Mono);

    ExportWavCommand surround;
    EXPECT_EQ(convert({ { "encoding", Val("PCM16") }, { "channels", Val("Surround") } }, surround).code(),
              static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathDefaultsToTheMacroOutputFolder)
{
    const RetVal<io::path_t> path = exportOutputPath("", SAVED_PROJECT, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/out/macro-output/song.wav"));
}

TEST(ExportCommandOptionsTests, PathReplacesTheVariablesAndAddsTheExtension)
{
    const RetVal<io::path_t> path = exportOutputPath("{ProjectDir}/{ProjectName}-hq", SAVED_PROJECT, "mp3");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/music/song-hq.mp3"));
}

TEST(ExportCommandOptionsTests, PathKeepsAnExistingExtension)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp/{ProjectName}.flac", SAVED_PROJECT, "flac");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/tmp/song.flac"));
}

TEST(ExportCommandOptionsTests, PathRejectsProjectDirForAnUnsavedProject)
{
    const RetVal<io::path_t> path = exportOutputPath("{ProjectDir}/out", UNSAVED_PROJECT, "ogg");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathRejectsARelativePath)
{
    const RetVal<io::path_t> path = exportOutputPath("out/{ProjectName}", SAVED_PROJECT, "wav");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}
