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

template<typename E, typename MakeSettings>
void expectEveryValueAccepted(const std::string& formatName, MakeSettings makeSettings,
                              const std::function<int(const std::string&)>& sampleRateFor = [] (const std::string&) { return 44100;
                              })
{
    for (const std::string& value : EnumContract<E>::str_vals()) {
        const RetVal<ExportParameters> parameters = au3ExportParameters(makeSettings(*EnumContract<E>::enum_val(value)),
                                                                        sampleRateFor(value));
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
    expectEveryValueAccepted<WavEncoding>("WAV (Microsoft)", [](WavEncoding e) { return WavSettings { e }; });
}

TEST(Au3ExportParametersTests, Mp3PresetsAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3Preset>("MP3 Files", [](Mp3Preset e) { return Mp3PresetSettings { e }; });
}

TEST(Au3ExportParametersTests, Mp3VbrQualitiesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3VbrQuality>("MP3 Files", [](Mp3VbrQuality e) { return Mp3VbrSettings { e }; });
}

TEST(Au3ExportParametersTests, Mp3AbrBitRatesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3BitRate>("MP3 Files", [](Mp3BitRate e) { return Mp3AbrSettings { e }; }, mp3BitRateSampleRate);
}

TEST(Au3ExportParametersTests, Mp3CbrBitRatesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<Mp3BitRate>("MP3 Files", [](Mp3BitRate e) { return Mp3CbrSettings { e }; }, mp3BitRateSampleRate);
}

TEST(Au3ExportParametersTests, FlacParametersAreAcceptedByThePlugin)
{
    for (const std::string& bitDepth : EnumContract<FlacBitDepth>::str_vals()) {
        for (const std::string& level : EnumContract<FlacLevel>::str_vals()) {
            const FlacSettings settings { *EnumContract<FlacBitDepth>::enum_val(bitDepth), *EnumContract<FlacLevel>::enum_val(level) };
            const RetVal<ExportParameters> parameters = au3ExportParameters(settings, 44100);
            ASSERT_TRUE(parameters.ret) << bitDepth << " " << level;
            expectAcceptedByPlugin("FLAC Files", parameters.val);
        }
    }
}

TEST(Au3ExportParametersTests, OggQualitiesAreAcceptedByThePlugin)
{
    expectEveryValueAccepted<OggQuality>("Ogg Vorbis Files", [](OggQuality e) { return OggSettings { e }; });
}

TEST(Au3ExportParametersTests, AudioDefaultsAreAcceptedByThePlugin)
{
    const std::vector<std::pair<ExportAudioFormat, std::string> > formats {
        { ExportAudioFormat::Wav, "WAV (Microsoft)" },
        { ExportAudioFormat::Mp3, "MP3 Files" },
        { ExportAudioFormat::Flac, "FLAC Files" },
        { ExportAudioFormat::Ogg, "Ogg Vorbis Files" },
    };

    for (const auto& [format, formatName] : formats) {
        const RetVal<ExportParameters> parameters = au3ExportParameters(defaultFormatSettings(format), 44100);
        ASSERT_TRUE(parameters.ret) << formatName;
        expectAcceptedByPlugin(formatName, parameters.val);
    }
}

TEST(Au3ExportParametersTests, AudioMp3UsesTheStandardPreset)
{
    const ExportFormatSettings settings = defaultFormatSettings(ExportAudioFormat::Mp3);
    EXPECT_EQ(extensionOf(settings), "mp3");

    const RetVal<ExportParameters> parameters = au3ExportParameters(settings, 44100);
    ASSERT_TRUE(parameters.ret);
    EXPECT_EQ(parameterValue(parameters.val, 1), static_cast<int>(Mp3Preset::Standard));
}

TEST(Au3ExportParametersTests, Mp3VbrUsesTheVbrQualityOption)
{
    const RetVal<ExportParameters> parameters = au3ExportParameters(Mp3VbrSettings { Mp3VbrQuality::V4 }, 44100);
    ASSERT_TRUE(parameters.ret);
    EXPECT_EQ(parameterValue(parameters.val, 2), 4);
    EXPECT_EQ(parameterValue(parameters.val, 4), -1);
}

TEST(Au3ExportParametersTests, Mp3RejectsAHighBitRateAtALowSampleRate)
{
    const RetVal<ExportParameters> parameters = au3ExportParameters(Mp3CbrSettings { Mp3BitRate::Kbps320 }, 22050);
    EXPECT_EQ(parameters.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}

TEST(Au3ExportParametersTests, Mp3RejectsASampleRateItDoesNotSupport)
{
    const RetVal<ExportParameters> parameters = au3ExportParameters(Mp3PresetSettings { Mp3Preset::Standard }, 96000);
    EXPECT_EQ(parameters.ret.code(), static_cast<int>(Ret::Code::BadArgs));
}
