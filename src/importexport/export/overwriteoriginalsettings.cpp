#include "overwriteoriginalsettings.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <type_traits>
#include <variant>

#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-strings/TranslatableString.h"

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

bool applyMp3CodecSettings(::ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    if (editor.GetName() != "mp3" || !codecSettings.contains("bitRate")) {
        return false;
    }

    const auto vbrPreset = Mp3VbrPresetForBitRate(codecSettings.value("bitRate").toDouble());
    if (!vbrPreset) {
        return false;
    }

    for (int index = 0; index < editor.GetOptionsCount(); ++index) {
        ::ExportOption option;
        if (!editor.GetOption(index, option)) {
            continue;
        }

        const bool isModeOption = std::any_of(option.values.cbegin(), option.values.cend(), [](const ::ExportValue& value) {
            const auto str = std::get_if<std::string>(&value);
            return str && *str == "VBR";
        });

        if (isModeOption) {
            editor.SetValue(option.id, std::string("VBR"));
            break;
        }
    }

    for (int index = 0; index < editor.GetOptionsCount(); ++index) {
        ::ExportOption option;
        if (!editor.GetOption(index, option)) {
            continue;
        }

        const bool isVisibleVbrQualityOption = (option.flags & ::ExportOption::Hidden) == 0
                                               && option.values.size() == 10
                                               && std::all_of(
            option.values.cbegin(), option.values.cend(), [](const ::ExportValue& value) {
            const auto numeric = std::get_if<int>(&value);
            return numeric && *numeric >= 0 && *numeric <= 9;
        });

        if (isVisibleVbrQualityOption) {
            editor.SetValue(option.id, *vbrPreset);
            return true;
        }
    }

    return true;
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

    const bool hasBitDepth = codecSettings.contains("bitDepth");
    const bool hasBitRate = codecSettings.contains("bitRate");
    const double bitDepth = codecSettings.value("bitDepth").toDouble();
    double bitRate = codecSettings.value("bitRate").toDouble();
    if (bitRate > 1000.0) {
        bitRate /= 1000.0;
    }

    for (int index = 0; index < editor.GetOptionsCount(); ++index) {
        ::ExportOption option;
        if (!editor.GetOption(index, option)) {
            continue;
        }

        const std::string title = option.title.Translation().Lower().ToStdString();
        const bool isBitDepth = title.find("bit depth") != std::string::npos;
        const bool isBitRate = title.find("bit rate") != std::string::npos
                               || title.find("bitrate") != std::string::npos
                               || title.find("quality") != std::string::npos;

        if (hasBitDepth && isBitDepth) {
            if (auto value = closestNumericOptionValue(option, bitDepth)) {
                editor.SetValue(option.id, *value);
            }
        } else if (hasBitRate && isBitRate) {
            const auto value = closestNumericOptionValue(option, bitRate);
            const auto numeric = value ? NumericExportValue(*value) : std::nullopt;
            if (value && numeric && *numeric >= 32.0) {
                editor.SetValue(option.id, *value);
            }
        }
    }
}
}
