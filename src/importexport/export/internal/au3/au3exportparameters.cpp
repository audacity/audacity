/*
* Audacity: A Digital Audio Editor
*/
#include "au3exportparameters.h"

#include <algorithm>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <sndfile.h>

#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginRegistry.h"

#include "framework/global/log.h"

#include "../exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
template<class ... Ts>
struct Overloaded : Ts ... {
    using Ts::operator() ...;
};

constexpr int WAV_ENCODING_OPTION_ID = SF_FORMAT_WAV;

constexpr int MP3_MODE_OPTION_ID = 0;
constexpr int MP3_PRESET_OPTION_ID = 1;
constexpr int MP3_VBR_QUALITY_OPTION_ID = 2;
constexpr int MP3_ABR_BIT_RATE_OPTION_ID = 3;
constexpr int MP3_CBR_BIT_RATE_OPTION_ID = 4;

constexpr int FLAC_BIT_DEPTH_OPTION_ID = 0;
constexpr int FLAC_LEVEL_OPTION_ID = 1;

constexpr int OGG_QUALITY_OPTION_ID = 1;

struct Mp3Option {
    std::string mode;
    int qualityOptionId = 0;
    int quality = 0;
};

struct OptionRef {
    std::string_view extension;
    int optionId = 0;
    ExportValue value;
};

int wavEncodingSubtype(WavEncoding encoding)
{
    switch (encoding) {
    case WavEncoding::Pcm16: return SF_FORMAT_PCM_16;
    case WavEncoding::Pcm24: return SF_FORMAT_PCM_24;
    case WavEncoding::Pcm32: return SF_FORMAT_PCM_32;
    case WavEncoding::Float32: return SF_FORMAT_FLOAT;
    }
    return SF_FORMAT_PCM_16;
}

std::unique_ptr<ExportOptionsEditor> formatOptionsEditor(std::string_view extension)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        const auto& extensions = plugin->GetFormatInfo(formatIndex).extensions;
        if (std::ranges::any_of(extensions, [extension](const auto& ext) { return ext.ToStdString() == extension; })) {
            return plugin->CreateOptionsEditor(formatIndex, nullptr);
        }
    }

    return nullptr;
}

std::vector<int> mp3SampleRates(const Mp3Option& option)
{
    const auto editor = formatOptionsEditor(extensionOf(Mp3PresetSettings {}));
    if (!editor) {
        LOGE() << "MP3 options editor not found";
        return {};
    }

    editor->SetValue(MP3_MODE_OPTION_ID, option.mode);
    editor->SetValue(option.qualityOptionId, option.quality);
    return editor->GetSampleRateList();
}

OptionRef optionRefOf(const ExportOptionValue& value)
{
    return std::visit(Overloaded {
            [](WavEncoding e) { return OptionRef { extensionOf(WavSettings {}), WAV_ENCODING_OPTION_ID, wavEncodingSubtype(e) }; },
            [](Mp3Preset p) { return OptionRef { extensionOf(Mp3PresetSettings {}), MP3_PRESET_OPTION_ID, static_cast<int>(p) }; },
            [](Mp3VbrQuality q) { return OptionRef { extensionOf(Mp3VbrSettings {}), MP3_VBR_QUALITY_OPTION_ID, static_cast<int>(q) }; },
            [](Mp3BitRate b) { return OptionRef { extensionOf(Mp3CbrSettings {}), MP3_CBR_BIT_RATE_OPTION_ID, static_cast<int>(b) }; },
            [](FlacBitDepth d) {
            return OptionRef { extensionOf(FlacSettings {}), FLAC_BIT_DEPTH_OPTION_ID, EnumContract<FlacBitDepth>::str_val(d) };
        },
            [](FlacLevel l) {
            return OptionRef { extensionOf(FlacSettings {}), FLAC_LEVEL_OPTION_ID, std::to_string(static_cast<int>(l)) };
        },
            [](OggQuality q) { return OptionRef { extensionOf(OggSettings {}), OGG_QUALITY_OPTION_ID, static_cast<int>(q) }; },
        }, value);
}

std::optional<::ExportOption> findOption(const ExportOptionsEditor& editor, int optionId)
{
    for (int i = 0; i < editor.GetOptionsCount(); ++i) {
        ::ExportOption option;
        if (editor.GetOption(i, option) && option.id == optionId) {
            return option;
        }
    }
    return std::nullopt;
}

using FormatOptions = std::variant<ExportParameters, Mp3Option>;

FormatOptions formatOptionsOf(const ExportFormatSettings& settings)
{
    return std::visit(Overloaded {
            [](const WavSettings& s) -> FormatOptions {
            return ExportParameters { { WAV_ENCODING_OPTION_ID, wavEncodingSubtype(s.encoding) } };
        },
            [](const Mp3PresetSettings& s) -> FormatOptions {
            return Mp3Option { "SET", MP3_PRESET_OPTION_ID, static_cast<int>(s.preset) };
        },
            [](const Mp3VbrSettings& s) -> FormatOptions {
            return Mp3Option { "VBR", MP3_VBR_QUALITY_OPTION_ID, static_cast<int>(s.quality) };
        },
            [](const Mp3AbrSettings& s) -> FormatOptions {
            return Mp3Option { "ABR", MP3_ABR_BIT_RATE_OPTION_ID, static_cast<int>(s.bitRate) };
        },
            [](const Mp3CbrSettings& s) -> FormatOptions {
            return Mp3Option { "CBR", MP3_CBR_BIT_RATE_OPTION_ID, static_cast<int>(s.bitRate) };
        },
            [](const FlacSettings& s) -> FormatOptions {
            return ExportParameters {
                { FLAC_BIT_DEPTH_OPTION_ID, EnumContract<FlacBitDepth>::str_val(s.bitDepth) },
                { FLAC_LEVEL_OPTION_ID, std::to_string(static_cast<int>(s.level)) },
            };
        },
            [](const OggSettings& s) -> FormatOptions {
            return ExportParameters { { OGG_QUALITY_OPTION_ID, static_cast<int>(s.quality) } };
        },
        }, settings);
}

RetVal<ExportParameters> mp3Parameters(const Mp3Option& option, int sampleRate)
{
    const std::vector<int> rates = mp3SampleRates(option);
    if (std::ranges::find(rates, sampleRate) == rates.end()) {
        LOGE() << "MP3 " << option.mode << " " << option.quality << " does not support the sample rate " << sampleRate;
        return RetVal<ExportParameters>::make_ret(Ret::Code::BadArgs);
    }

    return RetVal<ExportParameters>::make_ok({ { MP3_MODE_OPTION_ID, option.mode }, { option.qualityOptionId, option.quality } });
}
}

RetVal<ExportParameters> au::importexport::au3ExportParameters(const ExportFormatSettings& settings, int sampleRate)
{
    return std::visit(Overloaded {
        [](const ExportParameters& parameters) { return RetVal<ExportParameters>::make_ok(parameters); },
        [sampleRate](const Mp3Option& option) { return mp3Parameters(option, sampleRate); },
    }, formatOptionsOf(settings));
}

int au::importexport::au3DefaultSampleRate(const ExportFormatSettings& settings, int projectRate)
{
    const FormatOptions options = formatOptionsOf(settings);
    const Mp3Option* option = std::get_if<Mp3Option>(&options);
    if (!option) {
        return projectRate;
    }

    const std::vector<int> rates = mp3SampleRates(*option);
    if (rates.empty()) {
        return projectRate;
    }

    const auto nearest = std::ranges::lower_bound(rates, projectRate);
    return nearest == rates.end() ? rates.back() : *nearest;
}

std::optional<std::vector<int> > au::importexport::au3SupportedSampleRates(const ExportFormatSettings& settings)
{
    const FormatOptions options = formatOptionsOf(settings);
    const Mp3Option* option = std::get_if<Mp3Option>(&options);
    if (!option) {
        return std::nullopt;
    }

    return mp3SampleRates(*option);
}

std::optional<std::string> au::importexport::au3OptionLabel(const ExportOptionValue& value)
{
    const OptionRef ref = optionRefOf(value);
    const auto editor = formatOptionsEditor(ref.extension);
    if (!editor) {
        LOGE() << ref.extension << " options editor not found";
        return std::nullopt;
    }

    const std::optional<::ExportOption> option = findOption(*editor, ref.optionId);
    if (!option) {
        LOGE() << ref.extension << " has no option " << ref.optionId;
        return std::nullopt;
    }

    const auto it = std::ranges::find(option->values, ref.value);
    if (it == option->values.end()) {
        LOGE() << ref.extension << " option " << ref.optionId << " has no such value";
        return std::nullopt;
    }

    return option->names.at(static_cast<size_t>(std::distance(option->values.begin(), it))).translated().toStdString();
}
