/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <limits>
#include <string>
#include <string_view>
#include <tuple>

#include "framework/rcommand/commandtypes.h"
#include "framework/rcommand/enumcontract.h"
#include "framework/rcommand/typedcommand.h"

namespace au::importexport {
constexpr std::string_view EXPORT_COMMAND_PATH_PREFIX = "importexport/export/";
inline static const std::string EXPORT_NAME_PARAM("name");
inline static const std::string EXPORT_PATH_PARAM("path");
inline static const std::string EXPORT_SAMPLE_RATE_PARAM("sampleRate");

inline bool isExportCommand(const muse::rcommand::Command& command)
{
    return command.path().starts_with(EXPORT_COMMAND_PATH_PREFIX);
}

enum class ExportAudioFormat {
    Wav,
    Mp3,
    Flac,
    Ogg
};

struct ExportAudioFormatContract {
    static constexpr std::string_view key = "format";
    static constexpr ExportAudioFormat def = ExportAudioFormat::Wav;
    static constexpr muse::rcommand::EnumTable<ExportAudioFormat, 4> table = { {
        { "WAV", ExportAudioFormat::Wav, "WAV" },
        { "MP3", ExportAudioFormat::Mp3, "MP3" },
        { "FLAC", ExportAudioFormat::Flac, "FLAC" },
        { "OGG", ExportAudioFormat::Ogg, "Ogg Vorbis" },
    } };
};

enum class ExportChannelLayout {
    Mono,
    Stereo
};

struct ExportChannelLayoutContract {
    static constexpr std::string_view key = "channels";
    static constexpr ExportChannelLayout def = ExportChannelLayout::Stereo;
    static constexpr muse::rcommand::EnumTable<ExportChannelLayout, 2> table = { {
        { "Mono", ExportChannelLayout::Mono, "Mono" },
        { "Stereo", ExportChannelLayout::Stereo, "Stereo" },
    } };
};

enum class WavEncoding {
    Pcm16,
    Pcm24,
    Pcm32,
    Float32
};

struct WavEncodingContract {
    static constexpr std::string_view key = "encoding";
    static constexpr WavEncoding def = WavEncoding::Pcm16;
    static constexpr muse::rcommand::EnumTable<WavEncoding, 4> table = { {
        { "PCM16", WavEncoding::Pcm16, "Signed 16-bit PCM" },
        { "PCM24", WavEncoding::Pcm24, "Signed 24-bit PCM" },
        { "PCM32", WavEncoding::Pcm32, "Signed 32-bit PCM" },
        { "Float32", WavEncoding::Float32, "32-bit float" },
    } };
};

enum class Mp3Preset : int {
    Insane = 0,
    Extreme = 1,
    Standard = 2,
    Medium = 3
};

struct Mp3PresetContract {
    static constexpr std::string_view key = "quality";
    static constexpr Mp3Preset def = Mp3Preset::Standard;
    static constexpr muse::rcommand::EnumTable<Mp3Preset, 4> table = { {
        { "Insane", Mp3Preset::Insane, "Insane, 320 kbps" },
        { "Extreme", Mp3Preset::Extreme, "Extreme, 220-260 kbps" },
        { "Standard", Mp3Preset::Standard, "Standard, 170-210 kbps" },
        { "Medium", Mp3Preset::Medium, "Medium, 145-185 kbps" },
    } };
};

enum class Mp3VbrQuality : int {
    V0 = 0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9
};

struct Mp3VbrQualityContract {
    static constexpr std::string_view key = "quality";
    static constexpr Mp3VbrQuality def = Mp3VbrQuality::V2;
    static constexpr muse::rcommand::EnumTable<Mp3VbrQuality, 10> table = { {
        { "V0", Mp3VbrQuality::V0, "220-260 kbps (best quality)" },
        { "V1", Mp3VbrQuality::V1, "200-250 kbps" },
        { "V2", Mp3VbrQuality::V2, "170-210 kbps" },
        { "V3", Mp3VbrQuality::V3, "155-195 kbps" },
        { "V4", Mp3VbrQuality::V4, "145-185 kbps" },
        { "V5", Mp3VbrQuality::V5, "110-150 kbps" },
        { "V6", Mp3VbrQuality::V6, "95-135 kbps" },
        { "V7", Mp3VbrQuality::V7, "80-120 kbps" },
        { "V8", Mp3VbrQuality::V8, "65-105 kbps" },
        { "V9", Mp3VbrQuality::V9, "45-85 kbps (smaller files)" },
    } };
};

enum class Mp3BitRate : int {
    Kbps320 = 320,
    Kbps256 = 256,
    Kbps224 = 224,
    Kbps192 = 192,
    Kbps160 = 160,
    Kbps144 = 144,
    Kbps128 = 128,
    Kbps112 = 112,
    Kbps96 = 96,
    Kbps80 = 80,
    Kbps64 = 64,
    Kbps56 = 56,
    Kbps48 = 48,
    Kbps40 = 40,
    Kbps32 = 32,
    Kbps24 = 24,
    Kbps16 = 16,
    Kbps8 = 8
};

struct Mp3BitRateContract {
    static constexpr std::string_view key = "bitRate";
    static constexpr Mp3BitRate def = Mp3BitRate::Kbps192;
    static constexpr muse::rcommand::EnumTable<Mp3BitRate, 18> table = { {
        { "320", Mp3BitRate::Kbps320, "320 kbps" },
        { "256", Mp3BitRate::Kbps256, "256 kbps" },
        { "224", Mp3BitRate::Kbps224, "224 kbps" },
        { "192", Mp3BitRate::Kbps192, "192 kbps" },
        { "160", Mp3BitRate::Kbps160, "160 kbps" },
        { "144", Mp3BitRate::Kbps144, "144 kbps" },
        { "128", Mp3BitRate::Kbps128, "128 kbps" },
        { "112", Mp3BitRate::Kbps112, "112 kbps" },
        { "96", Mp3BitRate::Kbps96, "96 kbps" },
        { "80", Mp3BitRate::Kbps80, "80 kbps" },
        { "64", Mp3BitRate::Kbps64, "64 kbps" },
        { "56", Mp3BitRate::Kbps56, "56 kbps" },
        { "48", Mp3BitRate::Kbps48, "48 kbps" },
        { "40", Mp3BitRate::Kbps40, "40 kbps" },
        { "32", Mp3BitRate::Kbps32, "32 kbps" },
        { "24", Mp3BitRate::Kbps24, "24 kbps" },
        { "16", Mp3BitRate::Kbps16, "16 kbps" },
        { "8", Mp3BitRate::Kbps8, "8 kbps" },
    } };
};

enum class FlacBitDepth {
    Bits16,
    Bits24
};

struct FlacBitDepthContract {
    static constexpr std::string_view key = "bitDepth";
    static constexpr FlacBitDepth def = FlacBitDepth::Bits16;
    static constexpr muse::rcommand::EnumTable<FlacBitDepth, 2> table = { {
        { "16", FlacBitDepth::Bits16, "16 bit" },
        { "24", FlacBitDepth::Bits24, "24 bit" },
    } };
};

enum class FlacLevel : int {
    L0 = 0,
    L1,
    L2,
    L3,
    L4,
    L5,
    L6,
    L7,
    L8
};

struct FlacLevelContract {
    static constexpr std::string_view key = "level";
    static constexpr FlacLevel def = FlacLevel::L5;
    static constexpr muse::rcommand::EnumTable<FlacLevel, 9> table = { {
        { "0", FlacLevel::L0, "0 (fastest)" },
        { "1", FlacLevel::L1, "1" },
        { "2", FlacLevel::L2, "2" },
        { "3", FlacLevel::L3, "3" },
        { "4", FlacLevel::L4, "4" },
        { "5", FlacLevel::L5, "5" },
        { "6", FlacLevel::L6, "6" },
        { "7", FlacLevel::L7, "7" },
        { "8", FlacLevel::L8, "8 (smallest)" },
    } };
};

enum class OggQuality : int {
    Q0 = 0,
    Q1,
    Q2,
    Q3,
    Q4,
    Q5,
    Q6,
    Q7,
    Q8,
    Q9,
    Q10
};

struct OggQualityContract {
    static constexpr std::string_view key = "quality";
    static constexpr OggQuality def = OggQuality::Q5;
    static constexpr muse::rcommand::EnumTable<OggQuality, 11> table = { {
        { "0", OggQuality::Q0, "0 (~64 kbps)" },
        { "1", OggQuality::Q1, "1 (~80 kbps)" },
        { "2", OggQuality::Q2, "2 (~96 kbps)" },
        { "3", OggQuality::Q3, "3 (~112 kbps)" },
        { "4", OggQuality::Q4, "4 (~128 kbps)" },
        { "5", OggQuality::Q5, "5 (~160 kbps)" },
        { "6", OggQuality::Q6, "6 (~192 kbps)" },
        { "7", OggQuality::Q7, "7 (~224 kbps)" },
        { "8", OggQuality::Q8, "8 (~256 kbps)" },
        { "9", OggQuality::Q9, "9 (~320 kbps)" },
        { "10", OggQuality::Q10, "10 (~500 kbps)" },
    } };
};
}

namespace muse::rcommand {
DECLARE_ENUM_CONTRACT(au::importexport::ExportAudioFormat, au::importexport::ExportAudioFormatContract)
DECLARE_ENUM_CONTRACT(au::importexport::ExportChannelLayout, au::importexport::ExportChannelLayoutContract)
DECLARE_ENUM_CONTRACT(au::importexport::WavEncoding, au::importexport::WavEncodingContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3Preset, au::importexport::Mp3PresetContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3VbrQuality, au::importexport::Mp3VbrQualityContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3BitRate, au::importexport::Mp3BitRateContract)
DECLARE_ENUM_CONTRACT(au::importexport::FlacBitDepth, au::importexport::FlacBitDepthContract)
DECLARE_ENUM_CONTRACT(au::importexport::FlacLevel, au::importexport::FlacLevelContract)
DECLARE_ENUM_CONTRACT(au::importexport::OggQuality, au::importexport::OggQualityContract)
}

namespace au::importexport {
struct ExportAudioCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/audio");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export audio"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description",
                                                      "Export the project audio with the default settings of the chosen format");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    ExportAudioFormat format = ExportAudioFormatContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ ExportAudioFormatContract::key, &ExportAudioCommand::format,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Format"),
                               muse::TranslatableString::untranslatable("File format") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportAudioCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportAudioCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportAudioCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportAudioCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportWavCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/wav");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as WAV"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a WAV file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    WavEncoding encoding = WavEncodingContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ WavEncodingContract::key, &ExportWavCommand::encoding,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Encoding"),
                               muse::TranslatableString::untranslatable("Sample format of the WAV file") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportWavCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportWavCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportWavCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportWavCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportMp3PresetCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/mp3-preset");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3 (preset)"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as an MP3 file with a LAME preset");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    Mp3Preset quality = Mp3PresetContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ Mp3PresetContract::key, &ExportMp3PresetCommand::quality,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Quality"),
                               muse::TranslatableString::untranslatable("LAME preset") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportMp3PresetCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportMp3PresetCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportMp3PresetCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportMp3PresetCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportMp3VbrCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/mp3-vbr");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3 (variable bit rate)"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a variable bit rate MP3 file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    Mp3VbrQuality quality = Mp3VbrQualityContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ Mp3VbrQualityContract::key, &ExportMp3VbrCommand::quality,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Quality"),
                               muse::TranslatableString::untranslatable("V0 (best quality) to V9 (smallest file)") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportMp3VbrCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportMp3VbrCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportMp3VbrCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportMp3VbrCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportMp3AbrCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/mp3-abr");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3 (average bit rate)"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as an average bit rate MP3 file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    Mp3BitRate bitRate = Mp3BitRateContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ Mp3BitRateContract::key, &ExportMp3AbrCommand::bitRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Bit rate (kbps)"),
                               muse::TranslatableString::untranslatable("Average bit rate") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportMp3AbrCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportMp3AbrCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportMp3AbrCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportMp3AbrCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportMp3CbrCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/mp3-cbr");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3 (constant bit rate)"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a constant bit rate MP3 file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    Mp3BitRate bitRate = Mp3BitRateContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ Mp3BitRateContract::key, &ExportMp3CbrCommand::bitRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Bit rate (kbps)"),
                               muse::TranslatableString::untranslatable("Constant bit rate") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportMp3CbrCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportMp3CbrCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportMp3CbrCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportMp3CbrCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportFlacCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/flac");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as FLAC"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a FLAC file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    FlacBitDepth bitDepth = FlacBitDepthContract::def;
    FlacLevel level = FlacLevelContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ FlacBitDepthContract::key, &ExportFlacCommand::bitDepth,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Bit depth"),
                               muse::TranslatableString::untranslatable("Bits per sample") },
        muse::rcommand::Field{ FlacLevelContract::key, &ExportFlacCommand::level,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Level"),
                               muse::TranslatableString::untranslatable("Compression level, 0 (fastest) to 8 (smallest)") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportFlacCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportFlacCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportFlacCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportFlacCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};

struct ExportOggCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/ogg");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as OGG"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as an Ogg Vorbis file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    OggQuality quality = OggQualityContract::def;
    std::string path;
    int sampleRate = 0;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string name;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ OggQualityContract::key, &ExportOggCommand::quality,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Quality"),
                               muse::TranslatableString::untranslatable("0 (smallest) to 10 (best)") },
        muse::rcommand::Field{ EXPORT_PATH_PARAM, &ExportOggCommand::path,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Path"),
                               muse::TranslatableString::untranslatable(
                                   "Output file; can use {ProjectDir} and {ProjectName}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_SAMPLE_RATE_PARAM, &ExportOggCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate (Hz)"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               1, std::numeric_limits<int>::max(), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportOggCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_NAME_PARAM, &ExportOggCommand::name,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Default: the project name"), muse::rcommand::Presence::Optional },
    };
};
}
