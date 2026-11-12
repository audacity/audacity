/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <algorithm>
#include <functional>

#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginRegistry.h"

#include "importexport/export/internal/au3/au3exportparameters.h"
#include "importexport/export/internal/exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
std::optional<::ExportOption> pluginOption(const std::string& formatName, int id)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() != formatName) {
            continue;
        }

        const auto editor = plugin->CreateOptionsEditor(formatIndex, nullptr);
        for (int i = 0; i < editor->GetOptionsCount(); ++i) {
            ::ExportOption option;
            if (editor->GetOption(i, option) && option.id == id) {
                return option;
            }
        }
    }
    return std::nullopt;
}

void expectAcceptedByPlugin(const std::string& formatName, const ExportParameters& parameters)
{
    ASSERT_FALSE(parameters.empty());
    for (const auto& [id, value] : parameters) {
        const std::optional<::ExportOption> option = pluginOption(formatName, id);
        ASSERT_TRUE(option) << formatName << " has no option " << id;
        EXPECT_NE(std::ranges::find(option->values, value), option->values.end())
            << formatName << " option " << option->id << " does not accept the value";
    }
}

int parameterValue(const ExportParameters& parameters, int id)
{
    const auto it = std::ranges::find_if(parameters, [id](const auto& parameter) { return std::get<0>(parameter) == id; });
    return it == parameters.end() ? -1 : std::get<int>(std::get<1>(*it));
}

RetVal<ExportParameters> parametersFor(ExportCommandKind kind, const Params& params, int sampleRate = 44100)
{
    const RetVal<ExportFormatSettings> settings = exportFormatSettings(kind, params);
    if (!settings.ret) {
        return RetVal<ExportParameters>::make_ret(settings.ret);
    }
    return au3ExportParameters(settings.val, sampleRate);
}

template<typename E>
void expectEveryValueAccepted(ExportCommandKind kind, const std::string& key, const std::string& formatName,
                              const std::function<int(const std::string&)>& sampleRateFor = [] (const std::string&) { return 44100;
                              })
{
    for (const std::string& value : EnumContract<E>::str_vals()) {
        const RetVal<ExportParameters> parameters = parametersFor(kind, { { key, Val(value) } }, sampleRateFor(value));
        ASSERT_TRUE(parameters.ret) << formatName << " " << value;
        expectAcceptedByPlugin(formatName, parameters.val);
    }
}

int mp3BitRateSampleRate(const std::string& kbps)
{
    const int value = std::stoi(kbps);
    return value <= 31 || value == 144 ? 22050 : 44100;
}
}

TEST(Au3ExportParametersTests, WavEncodingsAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<WavEncoding>(ExportCommandKind::Wav, "encoding", "WAV (Microsoft)");
}

TEST(Au3ExportParametersTests, Mp3PresetsAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3Preset>(ExportCommandKind::Mp3Preset, "quality", "MP3 Files");
}

TEST(Au3ExportParametersTests, Mp3VbrQualitiesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3VbrQuality>(ExportCommandKind::Mp3Vbr, "quality", "MP3 Files");
}

TEST(Au3ExportParametersTests, Mp3AbrBitRatesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3BitRate>(ExportCommandKind::Mp3Abr, "bitRate", "MP3 Files", mp3BitRateSampleRate);
}

TEST(Au3ExportParametersTests, Mp3CbrBitRatesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3BitRate>(ExportCommandKind::Mp3Cbr, "bitRate", "MP3 Files", mp3BitRateSampleRate);
}

TEST(Au3ExportParametersTests, FlacParametersAreAcceptedByThePlugin)
{
    for (const std::string& bitDepth : EnumContract<FlacBitDepth>::str_vals()) {
        for (const std::string& level : EnumContract<FlacLevel>::str_vals()) {
            const RetVal<ExportParameters> parameters
                = parametersFor(ExportCommandKind::Flac, { { "bitDepth", Val(bitDepth) }, { "level", Val(level) } });
            ASSERT_TRUE(parameters.ret) << bitDepth << " " << level;
            expectAcceptedByPlugin("FLAC Files", parameters.val);
        }
    }
}

TEST(Au3ExportParametersTests, OggQualitiesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<OggQuality>(ExportCommandKind::Ogg, "quality", "Ogg Vorbis Files");
}

TEST(Au3ExportParametersTests, AudioDefaultsAreAcceptedByThePlugin)
{
    const std::vector<std::pair<std::string, std::string> > formats {
        { "WAV", "WAV (Microsoft)" }, { "MP3", "MP3 Files" }, { "FLAC", "FLAC Files" }, { "OGG", "Ogg Vorbis Files" },
    };

    for (const auto& [format, formatName] : formats) {
        const RetVal<ExportParameters> parameters = parametersFor(ExportCommandKind::Audio, { { "format", Val(format) } });
        ASSERT_TRUE(parameters.ret) << format;
        expectAcceptedByPlugin(formatName, parameters.val);
    }
}

TEST(Au3ExportParametersTests, AudioMp3UsesTheStandardPreset)
{
    const RetVal<ExportFormatSettings> settings = exportFormatSettings(ExportCommandKind::Audio, { { "format", Val("MP3") } });
    ASSERT_TRUE(settings.ret);
    EXPECT_EQ(extensionOf(settings.val), "mp3");

    const RetVal<ExportParameters> parameters = au3ExportParameters(settings.val, 44100);
    ASSERT_TRUE(parameters.ret);
    EXPECT_EQ(parameterValue(parameters.val, 1), static_cast<int>(Mp3Preset::Standard));
}

TEST(Au3ExportParametersTests, Mp3VbrUsesTheVbrQualityOption)
{
    const RetVal<ExportParameters> parameters = parametersFor(ExportCommandKind::Mp3Vbr, { { "quality", Val("V4") } });
    ASSERT_TRUE(parameters.ret);
    EXPECT_EQ(parameterValue(parameters.val, 2), 4);
    EXPECT_EQ(parameterValue(parameters.val, 4), -1);
}

TEST(Au3ExportParametersTests, Mp3RejectsAQualityThatIsNotInTheEnum)
{
    EXPECT_EQ(parametersFor(ExportCommandKind::Mp3Vbr, { { "quality", Val("V10") } }).ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(Au3ExportParametersTests, Mp3RejectsAHighBitRateAtALowSampleRate)
{
    const RetVal<ExportParameters> parameters = parametersFor(ExportCommandKind::Mp3Cbr, { { "bitRate", Val("320") } }, 22050);
    EXPECT_EQ(parameters.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(Au3ExportParametersTests, Mp3RejectsASampleRateItDoesNotSupport)
{
    const RetVal<ExportParameters> parameters = parametersFor(ExportCommandKind::Mp3Preset, { { "quality", Val("Standard") } }, 96000);
    EXPECT_EQ(parameters.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(Au3ExportParametersTests, FlacRejectsAMissingLevel)
{
    EXPECT_EQ(parametersFor(ExportCommandKind::Flac, { { "bitDepth", Val("16") } }).ret.code(), static_cast<int>(Ret::Code::BadArgs));
}
