#include "overwriteoriginalsettings.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <limits>
#include <string>
#include <type_traits>
#include <variant>

#include "au3-import-export/ExportOptionsEditor.h"

namespace au::importexport {
std::optional<double> NumericExportValue(const ::ExportValue& value)
{
    return std::visit([](const auto& v) -> std::optional<double> {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, int> || std::is_same_v<T, double>) {
            return static_cast<double>(v);
        } else if constexpr (std::is_same_v<T, std::string>) {
            try {
                size_t parsedLength = 0;
                const double parsed = std::stod(v, &parsedLength);
                return parsedLength == v.size() ? std::optional<double>(parsed) : std::nullopt;
            } catch (...) {
                return std::nullopt;
            }
        } else {
            return std::nullopt;
        }
    }, value);
}

namespace {
constexpr auto FLACFormatID = "FLAC";
constexpr auto FLACOptionIDBitDepth = 0;
constexpr auto MP3OptionIDMode = 0;
constexpr auto MP3OptionIDQualityVBR = 2;

std::optional<::ExportValue> closestNumericOptionValue(const ::ExportOption& option, double target)
{
    std::optional<::ExportValue> closest;
    double closestDistance = std::numeric_limits<double>::max();

    for (const auto& value : option.values) {
        const auto numeric = NumericExportValue(value);
        if (!numeric) {
            continue;
        }

        const double distance = std::abs(*numeric - target);
        if (distance < closestDistance) {
            closest = value;
            closestDistance = distance;
        }
    }

    return closest;
}

std::string normalizedFormatID(const QVariantMap& codecSettings)
{
    auto format = codecSettings.value("format").toString().toStdString();
    std::transform(format.begin(), format.end(), format.begin(), [](unsigned char c) {
        return static_cast<char>(std::toupper(c));
    });
    return format;
}

std::optional<::ExportOption> optionByID(::ExportOptionsEditor& editor, ::ExportOptionID optionID)
{
    for (int index = 0; index < editor.GetOptionsCount(); ++index) {
        ::ExportOption option;
        if (editor.GetOption(index, option) && option.id == optionID) {
            return option;
        }
    }

    return std::nullopt;
}

bool setClosestNumericOptionValue(::ExportOptionsEditor& editor, ::ExportOptionID optionID, double target)
{
    const auto option = optionByID(editor, optionID);
    if (!option) {
        return false;
    }

    const auto value = closestNumericOptionValue(*option, target);
    if (!value) {
        return false;
    }

    return editor.SetValue(option->id, *value);
}

bool applyMp3CodecSettings(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    // MP3ExportOptionsEditor::GetName() returns "mp3"; its option ids are
    // MP3OptionIDMode == 0 and MP3OptionIDQualityVBR == 2 in ExportMP3.cpp.
    if (editor.GetName() != "mp3" || !codecSettings.contains("bitRate")) {
        return false;
    }

    const auto vbrPreset = Mp3VbrPresetForBitRate(codecSettings.value("bitRate").toDouble());
    if (!vbrPreset) {
        return false;
    }

    editor.SetValue(MP3OptionIDMode, std::string("VBR"));
    editor.SetValue(MP3OptionIDQualityVBR, *vbrPreset);
    return true;
}

bool applyFlacCodecSettings(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    if (normalizedFormatID(codecSettings) != FLACFormatID || !codecSettings.contains("bitDepth")) {
        return false;
    }

    // ExportFLAC exposes FlacOptionIDBitDepth as option id 0.
    return setClosestNumericOptionValue(editor, FLACOptionIDBitDepth, codecSettings.value("bitDepth").toDouble());
}
}

std::optional<int> Mp3VbrPresetForBitRate(double bitRate)
{
    if (bitRate <= 0.0) {
        return std::nullopt;
    }

    const double kbps = bitRate > 1000.0 ? bitRate / 1000.0 : bitRate;
    if (kbps >= 230.0) {
        return 0;
    }
    if (kbps >= 200.0) {
        return 1;
    }
    if (kbps >= 180.0) {
        return 2;
    }
    if (kbps >= 170.0) {
        return 3;
    }
    if (kbps >= 140.0) {
        return 4;
    }
    if (kbps >= 120.0) {
        return 5;
    }
    return 6;
}

void ApplyCodecSettingsToExportOptions(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    if (applyMp3CodecSettings(editor, codecSettings)) {
        return;
    }

    applyFlacCodecSettings(editor, codecSettings);
}
}
