/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <string_view>
#include <tuple>

#include "framework/rcommand/commandtypes.h"
#include "framework/rcommand/enumcontract.h"
#include "framework/rcommand/typedcommand.h"

namespace au::importexport {
inline static const std::string EXPORT_FOLDER_PARAM("folder");
inline static const std::string EXPORT_FILE_NAME_PARAM("fileName");
inline static const std::string EXPORT_PROJECT_NAME_VARIABLE("{ProjectName}");

enum class ExportChannelLayout {
    Auto,
    Mono,
    Stereo
};

struct ExportChannelLayoutContract {
    static constexpr std::string_view key = "channels";
    static constexpr ExportChannelLayout def = ExportChannelLayout::Auto;
    static constexpr muse::rcommand::EnumTable<ExportChannelLayout, 3> table = { {
        { "Auto", ExportChannelLayout::Auto, "Auto" },
        { "Mono", ExportChannelLayout::Mono, "Mono" },
        { "Stereo", ExportChannelLayout::Stereo, "Stereo" },
    } };
};

enum class ExportSampleRate : int {
    ProjectRate = 0,
    Hz8000 = 8000,
    Hz11025 = 11025,
    Hz16000 = 16000,
    Hz22050 = 22050,
    Hz32000 = 32000,
    Hz44100 = 44100,
    Hz48000 = 48000,
    Hz88200 = 88200,
    Hz96000 = 96000,
    Hz176400 = 176400,
    Hz192000 = 192000,
    Hz352800 = 352800,
    Hz384000 = 384000
};

struct ExportSampleRateContract {
    static constexpr std::string_view key = "sampleRate";
    static constexpr ExportSampleRate def = ExportSampleRate::ProjectRate;
    static constexpr muse::rcommand::EnumTable<ExportSampleRate, 14> table = { {
        { "project", ExportSampleRate::ProjectRate, "Project rate" },
        { "8000", ExportSampleRate::Hz8000, "8000 Hz" },
        { "11025", ExportSampleRate::Hz11025, "11025 Hz" },
        { "16000", ExportSampleRate::Hz16000, "16000 Hz" },
        { "22050", ExportSampleRate::Hz22050, "22050 Hz" },
        { "32000", ExportSampleRate::Hz32000, "32000 Hz" },
        { "44100", ExportSampleRate::Hz44100, "44100 Hz" },
        { "48000", ExportSampleRate::Hz48000, "48000 Hz" },
        { "88200", ExportSampleRate::Hz88200, "88200 Hz" },
        { "96000", ExportSampleRate::Hz96000, "96000 Hz" },
        { "176400", ExportSampleRate::Hz176400, "176400 Hz" },
        { "192000", ExportSampleRate::Hz192000, "192000 Hz" },
        { "352800", ExportSampleRate::Hz352800, "352800 Hz" },
        { "384000", ExportSampleRate::Hz384000, "384000 Hz" },
    } };
};

enum class Mp3SampleRate : int {
    ProjectRate = 0,
    Hz8000 = 8000,
    Hz11025 = 11025,
    Hz12000 = 12000,
    Hz16000 = 16000,
    Hz22050 = 22050,
    Hz24000 = 24000,
    Hz32000 = 32000,
    Hz44100 = 44100,
    Hz48000 = 48000
};

struct Mp3SampleRateContract {
    static constexpr std::string_view key = "sampleRate";
    static constexpr Mp3SampleRate def = Mp3SampleRate::ProjectRate;
    static constexpr muse::rcommand::EnumTable<Mp3SampleRate, 10> table = { {
        { "project", Mp3SampleRate::ProjectRate, "Project rate" },
        { "8000", Mp3SampleRate::Hz8000, "8000 Hz" },
        { "11025", Mp3SampleRate::Hz11025, "11025 Hz" },
        { "12000", Mp3SampleRate::Hz12000, "12000 Hz" },
        { "16000", Mp3SampleRate::Hz16000, "16000 Hz" },
        { "22050", Mp3SampleRate::Hz22050, "22050 Hz" },
        { "24000", Mp3SampleRate::Hz24000, "24000 Hz" },
        { "32000", Mp3SampleRate::Hz32000, "32000 Hz" },
        { "44100", Mp3SampleRate::Hz44100, "44100 Hz" },
        { "48000", Mp3SampleRate::Hz48000, "48000 Hz" },
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

constexpr int MP3_PRESET_BASE = 1000;
constexpr int MP3_VBR_BASE = 2000;
constexpr int MP3_ABR_BASE = 3000;
constexpr int MP3_CBR_BASE = 4000;

enum class Mp3Quality : int {
    PresetInsane = MP3_PRESET_BASE + static_cast<int>(Mp3Preset::Insane),
    PresetExtreme = MP3_PRESET_BASE + static_cast<int>(Mp3Preset::Extreme),
    PresetStandard = MP3_PRESET_BASE + static_cast<int>(Mp3Preset::Standard),
    PresetMedium = MP3_PRESET_BASE + static_cast<int>(Mp3Preset::Medium),
    VbrV0 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V0),
    VbrV1 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V1),
    VbrV2 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V2),
    VbrV3 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V3),
    VbrV4 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V4),
    VbrV5 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V5),
    VbrV6 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V6),
    VbrV7 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V7),
    VbrV8 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V8),
    VbrV9 = MP3_VBR_BASE + static_cast<int>(Mp3VbrQuality::V9),
    Abr320 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps320),
    Abr256 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps256),
    Abr224 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps224),
    Abr192 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps192),
    Abr160 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps160),
    Abr144 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps144),
    Abr128 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps128),
    Abr112 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps112),
    Abr96 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps96),
    Abr80 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps80),
    Abr64 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps64),
    Abr56 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps56),
    Abr48 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps48),
    Abr40 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps40),
    Abr32 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps32),
    Abr24 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps24),
    Abr16 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps16),
    Abr8 = MP3_ABR_BASE + static_cast<int>(Mp3BitRate::Kbps8),
    Cbr320 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps320),
    Cbr256 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps256),
    Cbr224 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps224),
    Cbr192 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps192),
    Cbr160 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps160),
    Cbr144 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps144),
    Cbr128 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps128),
    Cbr112 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps112),
    Cbr96 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps96),
    Cbr80 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps80),
    Cbr64 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps64),
    Cbr56 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps56),
    Cbr48 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps48),
    Cbr40 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps40),
    Cbr32 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps32),
    Cbr24 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps24),
    Cbr16 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps16),
    Cbr8 = MP3_CBR_BASE + static_cast<int>(Mp3BitRate::Kbps8)
};

struct Mp3QualityContract {
    static constexpr std::string_view key = "quality";
    static constexpr Mp3Quality def = Mp3Quality::PresetStandard;
    static constexpr muse::rcommand::EnumTable<Mp3Quality, 50> table = { {
        { "preset-insane", Mp3Quality::PresetInsane, "Preset: Insane, 320 kbps" },
        { "preset-extreme", Mp3Quality::PresetExtreme, "Preset: Extreme, 220-260 kbps" },
        { "preset-standard", Mp3Quality::PresetStandard, "Preset: Standard, 170-210 kbps" },
        { "preset-medium", Mp3Quality::PresetMedium, "Preset: Medium, 145-185 kbps" },
        { "vbr-v0", Mp3Quality::VbrV0, "Variable: V0, 220-260 kbps, best quality" },
        { "vbr-v1", Mp3Quality::VbrV1, "Variable: V1, 200-250 kbps" },
        { "vbr-v2", Mp3Quality::VbrV2, "Variable: V2, 170-210 kbps" },
        { "vbr-v3", Mp3Quality::VbrV3, "Variable: V3, 155-195 kbps" },
        { "vbr-v4", Mp3Quality::VbrV4, "Variable: V4, 145-185 kbps" },
        { "vbr-v5", Mp3Quality::VbrV5, "Variable: V5, 110-150 kbps" },
        { "vbr-v6", Mp3Quality::VbrV6, "Variable: V6, 95-135 kbps" },
        { "vbr-v7", Mp3Quality::VbrV7, "Variable: V7, 80-120 kbps" },
        { "vbr-v8", Mp3Quality::VbrV8, "Variable: V8, 65-105 kbps" },
        { "vbr-v9", Mp3Quality::VbrV9, "Variable: V9, 45-85 kbps, smallest file" },
        { "abr-320", Mp3Quality::Abr320, "Average: 320 kbps" },
        { "abr-256", Mp3Quality::Abr256, "Average: 256 kbps" },
        { "abr-224", Mp3Quality::Abr224, "Average: 224 kbps" },
        { "abr-192", Mp3Quality::Abr192, "Average: 192 kbps" },
        { "abr-160", Mp3Quality::Abr160, "Average: 160 kbps" },
        { "abr-144", Mp3Quality::Abr144, "Average: 144 kbps" },
        { "abr-128", Mp3Quality::Abr128, "Average: 128 kbps" },
        { "abr-112", Mp3Quality::Abr112, "Average: 112 kbps" },
        { "abr-96", Mp3Quality::Abr96, "Average: 96 kbps" },
        { "abr-80", Mp3Quality::Abr80, "Average: 80 kbps" },
        { "abr-64", Mp3Quality::Abr64, "Average: 64 kbps" },
        { "abr-56", Mp3Quality::Abr56, "Average: 56 kbps" },
        { "abr-48", Mp3Quality::Abr48, "Average: 48 kbps" },
        { "abr-40", Mp3Quality::Abr40, "Average: 40 kbps" },
        { "abr-32", Mp3Quality::Abr32, "Average: 32 kbps" },
        { "abr-24", Mp3Quality::Abr24, "Average: 24 kbps" },
        { "abr-16", Mp3Quality::Abr16, "Average: 16 kbps" },
        { "abr-8", Mp3Quality::Abr8, "Average: 8 kbps" },
        { "cbr-320", Mp3Quality::Cbr320, "Constant: 320 kbps" },
        { "cbr-256", Mp3Quality::Cbr256, "Constant: 256 kbps" },
        { "cbr-224", Mp3Quality::Cbr224, "Constant: 224 kbps" },
        { "cbr-192", Mp3Quality::Cbr192, "Constant: 192 kbps" },
        { "cbr-160", Mp3Quality::Cbr160, "Constant: 160 kbps" },
        { "cbr-144", Mp3Quality::Cbr144, "Constant: 144 kbps" },
        { "cbr-128", Mp3Quality::Cbr128, "Constant: 128 kbps" },
        { "cbr-112", Mp3Quality::Cbr112, "Constant: 112 kbps" },
        { "cbr-96", Mp3Quality::Cbr96, "Constant: 96 kbps" },
        { "cbr-80", Mp3Quality::Cbr80, "Constant: 80 kbps" },
        { "cbr-64", Mp3Quality::Cbr64, "Constant: 64 kbps" },
        { "cbr-56", Mp3Quality::Cbr56, "Constant: 56 kbps" },
        { "cbr-48", Mp3Quality::Cbr48, "Constant: 48 kbps" },
        { "cbr-40", Mp3Quality::Cbr40, "Constant: 40 kbps" },
        { "cbr-32", Mp3Quality::Cbr32, "Constant: 32 kbps" },
        { "cbr-24", Mp3Quality::Cbr24, "Constant: 24 kbps" },
        { "cbr-16", Mp3Quality::Cbr16, "Constant: 16 kbps" },
        { "cbr-8", Mp3Quality::Cbr8, "Constant: 8 kbps" },
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
    static constexpr OggQuality def = OggQuality::Q6;
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

enum class ExportPreset {
    Lossless,
    HighQuality,
    Standard,
    SmallFile,
    Speech
};

struct ExportPresetContract {
    static constexpr std::string_view key = "preset";
    static constexpr ExportPreset def = ExportPreset::Standard;
    static constexpr muse::rcommand::EnumTable<ExportPreset, 5> table = { {
        { "lossless", ExportPreset::Lossless, "Lossless (FLAC)" },
        { "high", ExportPreset::HighQuality, "High quality" },
        { "standard", ExportPreset::Standard, "Standard" },
        { "small", ExportPreset::SmallFile, "Small file" },
        { "speech", ExportPreset::Speech, "Speech" },
    } };
};
}

namespace muse::rcommand {
DECLARE_ENUM_CONTRACT(au::importexport::ExportChannelLayout, au::importexport::ExportChannelLayoutContract)
DECLARE_ENUM_CONTRACT(au::importexport::ExportSampleRate, au::importexport::ExportSampleRateContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3SampleRate, au::importexport::Mp3SampleRateContract)
DECLARE_ENUM_CONTRACT(au::importexport::WavEncoding, au::importexport::WavEncodingContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3Preset, au::importexport::Mp3PresetContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3VbrQuality, au::importexport::Mp3VbrQualityContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3BitRate, au::importexport::Mp3BitRateContract)
DECLARE_ENUM_CONTRACT(au::importexport::Mp3Quality, au::importexport::Mp3QualityContract)
DECLARE_ENUM_CONTRACT(au::importexport::FlacBitDepth, au::importexport::FlacBitDepthContract)
DECLARE_ENUM_CONTRACT(au::importexport::FlacLevel, au::importexport::FlacLevelContract)
DECLARE_ENUM_CONTRACT(au::importexport::OggQuality, au::importexport::OggQualityContract)
DECLARE_ENUM_CONTRACT(au::importexport::ExportPreset, au::importexport::ExportPresetContract)
}

namespace au::importexport {
struct ExportAsWavCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/wav");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as WAV"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description",
                                                      "Export the project audio as a WAV file with the options saved in the Export dialog");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportAsWavCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportAsWavCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"),
                               muse::rcommand::Presence::Optional },
    };
};

struct ExportAsMp3Command {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/mp3");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description",
                                                      "Export the project audio as an MP3 file with the options saved in the Export dialog");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportAsMp3Command::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportAsMp3Command::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"),
                               muse::rcommand::Presence::Optional },
    };
};

struct ExportAsFlacCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/flac");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as FLAC"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description",
                                                      "Export the project audio as a FLAC file with the options saved in the Export dialog");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportAsFlacCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportAsFlacCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"),
                               muse::rcommand::Presence::Optional },
    };
};

struct ExportAsOggCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/ogg");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as OGG"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description",
                                                      "Export the project audio as an Ogg Vorbis file with the options saved in the Export dialog");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::API;

    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportAsOggCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportAsOggCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"),
                               muse::rcommand::Presence::Optional },
    };
};

struct ExportWavCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/settings/wav");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as WAV with settings"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a WAV file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::MCP;

    WavEncoding encoding = WavEncodingContract::def;
    ExportSampleRate sampleRate = ExportSampleRateContract::def;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ WavEncodingContract::key, &ExportWavCommand::encoding,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Encoding"),
                               muse::TranslatableString::untranslatable("Sample format of the WAV file. Default: signed 16-bit PCM"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportSampleRateContract::key, &ExportWavCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportWavCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: auto, mono when no track is stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportWavCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportWavCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"), muse::rcommand::Presence::Optional },
    };
};

struct ExportMp3Command {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/settings/mp3");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as MP3 with settings"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as an MP3 file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::MCP;

    Mp3Quality quality = Mp3QualityContract::def;
    Mp3SampleRate sampleRate = Mp3SampleRateContract::def;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ Mp3QualityContract::key, &ExportMp3Command::quality,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Quality"),
                               muse::TranslatableString::untranslatable("Bit rate mode and quality. Default: preset Standard"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ Mp3SampleRateContract::key, &ExportMp3Command::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate, or the nearest rate that MP3 supports"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportMp3Command::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: auto, mono when no track is stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportMp3Command::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportMp3Command::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"),
                               muse::rcommand::Presence::Optional },
    };
};

struct ExportFlacCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/settings/flac");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as FLAC with settings"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as a FLAC file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::MCP;

    FlacBitDepth bitDepth = FlacBitDepthContract::def;
    FlacLevel level = FlacLevelContract::def;
    ExportSampleRate sampleRate = ExportSampleRateContract::def;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ FlacBitDepthContract::key, &ExportFlacCommand::bitDepth,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Bit depth"),
                               muse::TranslatableString::untranslatable("Bits per sample. Default: 16 bit"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ FlacLevelContract::key, &ExportFlacCommand::level,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Level"),
                               muse::TranslatableString::untranslatable("Compression level, 0 (fastest) to 8 (smallest). Default: 5"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportSampleRateContract::key, &ExportFlacCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportFlacCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: auto, mono when no track is stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportFlacCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportFlacCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"), muse::rcommand::Presence::Optional },
    };
};

struct ExportOggCommand {
    cprop_decl id = muse::rcommand::Command("command://importexport/export/settings/ogg");
    //: Command title: shown as a menu item or a button label; keep it short
    cprop_decl title = muse::MnemonicString(muse::TranslatableString("command", "Export as OGG with settings"));
    //: Command description: shown as a tooltip; can be a full sentence
    cprop_decl description = muse::TranslatableString("command_description", "Export the project audio as an Ogg Vorbis file");
    cprop_decl availabilities = muse::rcommand::Availability::Internal | muse::rcommand::Availability::MCP;

    OggQuality quality = OggQualityContract::def;
    ExportSampleRate sampleRate = ExportSampleRateContract::def;
    ExportChannelLayout channels = ExportChannelLayoutContract::def;
    std::string folder;
    std::string fileName;

    cprop_decl input = std::tuple {
        muse::rcommand::Field{ OggQualityContract::key, &ExportOggCommand::quality,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Quality"),
                               muse::TranslatableString::untranslatable("0 (smallest) to 10 (best). Default: 6"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportSampleRateContract::key, &ExportOggCommand::sampleRate,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Sample rate"),
                               muse::TranslatableString::untranslatable("Default: the project sample rate"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ ExportChannelLayoutContract::key, &ExportOggCommand::channels,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Channels"),
                               muse::TranslatableString::untranslatable("Default: auto, mono when no track is stereo"), muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FOLDER_PARAM, &ExportOggCommand::folder,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "Folder"),
                               muse::TranslatableString::untranslatable("Output folder; can use {ProjectDir}. Default: the macro-output folder"),
                               muse::rcommand::Presence::Optional },
        muse::rcommand::Field{ EXPORT_FILE_NAME_PARAM, &ExportOggCommand::fileName,
                               //: Command param label: shown next to the field in the command params dialog
                               muse::TranslatableString("command_param", "File name"),
                               muse::TranslatableString::untranslatable("Can use {ProjectName}. Default: {ProjectName}"), muse::rcommand::Presence::Optional },
    };
};
}
