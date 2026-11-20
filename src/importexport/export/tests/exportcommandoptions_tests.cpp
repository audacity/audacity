/*
* Audacity: A Digital Audio Editor
*/
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <QString>

#include "importexport/export/internal/exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
const OutputPathContext SAVED_PROJECT { io::path_t("/out/macro-output"), io::path_t("/music"), "song" };
const OutputPathContext UNSAVED_PROJECT { io::path_t("/out/macro-output"), std::nullopt, "song" };
const OutputPathContext MACRO_FILE { io::path_t("/out/macro-output"), io::path_t("/music"), "song", "take-2" };

std::string lower(const std::string& text)
{
    return QString::fromStdString(text).toLower().toStdString();
}

std::string settingsKey(const ExportFormatSettings& settings)
{
    if (const auto* preset = std::get_if<Mp3PresetSettings>(&settings)) {
        return "preset-" + lower(EnumContract<Mp3Preset>::str_val(preset->preset));
    }
    if (const auto* vbr = std::get_if<Mp3VbrSettings>(&settings)) {
        return "vbr-" + lower(EnumContract<Mp3VbrQuality>::str_val(vbr->quality));
    }
    if (const auto* abr = std::get_if<Mp3AbrSettings>(&settings)) {
        return "abr-" + EnumContract<Mp3BitRate>::str_val(abr->bitRate);
    }
    if (const auto* cbr = std::get_if<Mp3CbrSettings>(&settings)) {
        return "cbr-" + EnumContract<Mp3BitRate>::str_val(cbr->bitRate);
    }
    return {};
}

template<typename C>
Ret convert(const Params& params, C& command)
{
    return fromInputParams(params, command).ret;
}
}

TEST(ExportCommandOptionsTests, Mp3RejectsAQualityThatIsNotInTheEnum)
{
    ExportMp3Command command;
    EXPECT_EQ(convert({ { "quality", Val("vbr-v10") } }, command).code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, FlacUsesTheDefaultLevelWhenNotGiven)
{
    ExportFlacCommand command;
    ASSERT_TRUE(convert({ { "bitDepth", Val("24") } }, command));
    EXPECT_EQ(command.bitDepth, FlacBitDepth::Bits24);
    EXPECT_EQ(command.level, FlacLevel::L5);
}

TEST(ExportCommandOptionsTests, SampleRateIsTheProjectRateWhenNotGivenAndMustBeInTheList)
{
    ExportWavCommand notGiven;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") } }, notGiven));
    EXPECT_EQ(notGiven.sampleRate, ExportSampleRate::ProjectRate);

    ExportWavCommand given;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") }, { "sampleRate", Val("22050") } }, given));
    EXPECT_EQ(given.sampleRate, ExportSampleRate::Hz22050);

    ExportWavCommand notInTheList;
    EXPECT_EQ(convert({ { "encoding", Val("PCM16") }, { "sampleRate", Val("12345") } }, notInTheList).code(),
              static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, ChannelsDefaultToAuto)
{
    ExportWavCommand notGiven;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") } }, notGiven));
    EXPECT_EQ(notGiven.channels, ExportChannelLayout::Auto);

    ExportWavCommand mono;
    ASSERT_TRUE(convert({ { "encoding", Val("PCM16") }, { "channels", Val("Mono") } }, mono));
    EXPECT_EQ(mono.channels, ExportChannelLayout::Mono);

    ExportWavCommand surround;
    EXPECT_EQ(convert({ { "encoding", Val("PCM16") }, { "channels", Val("Surround") } }, surround).code(),
              static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathDefaultsToTheMacroOutputFolderAndTheProjectName)
{
    const RetVal<io::path_t> path = exportOutputPath("", "", SAVED_PROJECT, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/out/macro-output/song.wav"));
}

TEST(ExportCommandOptionsTests, PathReplacesTheVariablesAndAddsTheExtension)
{
    const RetVal<io::path_t> path = exportOutputPath("{ProjectDir}/exports", "{ProjectName}-hq", SAVED_PROJECT, "mp3");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/music/exports/song-hq.mp3"));
}

TEST(ExportCommandOptionsTests, PathKeepsAnExistingExtension)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp", "{ProjectName}.flac", SAVED_PROJECT, "flac");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/tmp/song.flac"));
}

TEST(ExportCommandOptionsTests, PathEscapesTheFileName)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp", "a/b", SAVED_PROJECT, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(io::dirpath(path.val), io::path_t("/tmp"));
}

TEST(ExportCommandOptionsTests, PathRejectsProjectDirForAnUnsavedProject)
{
    const RetVal<io::path_t> path = exportOutputPath("{ProjectDir}/out", "", UNSAVED_PROJECT, "ogg");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathRejectsARelativeFolder)
{
    const RetVal<io::path_t> path = exportOutputPath("out", "", SAVED_PROJECT, "wav");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(ExportCommandOptionsTests, PathUsesTheMacroFileNameWhenNoFileNameIsGiven)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp", "", MACRO_FILE, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/tmp/take-2.wav"));
}

TEST(ExportCommandOptionsTests, PathReplacesEveryProjectNameWithTheMacroFileName)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp", "{ProjectName}-hq-{ProjectName}", MACRO_FILE, "wav");
    ASSERT_TRUE(path.ret);
    EXPECT_EQ(path.val, io::path_t("/tmp/take-2-hq-take-2.wav"));
}

TEST(ExportCommandOptionsTests, PathRejectsAFileNameWithoutProjectNameForAMacroFile)
{
    const RetVal<io::path_t> path = exportOutputPath("/tmp", "mix", MACRO_FILE, "wav");
    EXPECT_EQ(path.ret.code(), static_cast<int>(Ret::Code::BadArgs));
    EXPECT_THAT(path.ret.text(), ::testing::HasSubstr("{ProjectName}"));
}

TEST(ExportCommandOptionsTests, EveryMp3QualityMapsToTheSettingsItsKeyNames)
{
    for (const auto& entry : Mp3QualityContract::table) {
        EXPECT_EQ(settingsKey(mp3Settings(entry.eval)), entry.sval);
    }
}

TEST(ExportCommandOptionsTests, SettingsMapBackToTheirMp3Quality)
{
    for (const auto& entry : Mp3QualityContract::table) {
        EXPECT_EQ(mp3QualityOf(mp3Settings(entry.eval)), entry.eval) << entry.sval;
    }

    EXPECT_EQ(mp3QualityOf(WavSettings {}), std::nullopt);
}
