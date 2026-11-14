/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorUtils.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorUtils.h"
#include <optional>
#include <regex>
#include <stdexcept>
#include <unordered_map>

namespace DynamicRangeProcessorUtils {
namespace {
template<typename Struct> struct SettingDescription
{
    double Struct::* const mem {};
    const char* const key {};
};

static const std::vector<SettingDescription<CompressorSettings> >
compressorSettingDescriptions {
    { &CompressorSettings::thresholdDb, "thresholdDb" },
    { &CompressorSettings::makeupGainDb, "makeupGainDb" },
    { &CompressorSettings::kneeWidthDb, "kneeWidthDb" },
    { &CompressorSettings::compressionRatio, "compressionRatio" },
    { &CompressorSettings::lookaheadMs, "lookaheadMs" },
    { &CompressorSettings::attackMs, "attackMs" },
    { &CompressorSettings::releaseMs, "releaseMs" },
    { &CompressorSettings::showInput, "showInput" },
    { &CompressorSettings::showOutput, "showOutput" },
    { &CompressorSettings::showActual, "showActual" },
    { &CompressorSettings::showTarget, "showTarget" },
};

const std::vector<SettingDescription<LimiterSettings> >
limiterSettingDescriptions {
    { &LimiterSettings::thresholdDb, "thresholdDb" },
    { &LimiterSettings::makeupTargetDb, "makeupTargetDb" },
    { &LimiterSettings::kneeWidthDb, "kneeWidthDb" },
    { &LimiterSettings::lookaheadMs, "lookaheadMs" },
    { &LimiterSettings::releaseMs, "releaseMs" },
    { &LimiterSettings::showInput, "showInput" },
    { &LimiterSettings::showOutput, "showOutput" },
    { &LimiterSettings::showActual, "showActual" },
    { &LimiterSettings::showTarget, "showTarget" },
};

template<typename Struct>
std::optional<Struct> Deserialize(
    const std::string& str,
    const std::vector<SettingDescription<Struct> >& settings)
{
    Struct settingStruct;

    const std::regex pattern(R"((\w+)=\"(-?\d+\.?\d*)\")");

    // Create an unordered_map to store the key-value pairs
    std::unordered_map<std::string, std::string> values;

    const auto begin = std::sregex_iterator(str.begin(), str.end(), pattern);
    const auto end = std::sregex_iterator();

    for (auto it = begin; it != end; ++it) {
        const std::smatch match = *it;
        values[match[1].str()] = match[2].str();
    }

    if (std::any_of(
            settings.begin(), settings.end(), [&values](const auto& setting) {
        return values.find(setting.key) == values.end();
    })) {
        return {};
    }

    for (const auto& setting : settings) {
        try
        {
            settingStruct.*(setting.mem) = std::stod(values.at(setting.key));
        }
        catch (...)
        {
            return {};
        }
    }

    return settingStruct;
}

template<typename Struct>
std::vector<Preset<Struct> > GetPresets(
    const std::vector<SettingDescription<Struct> >& settingDescriptions,
    const std::vector<Detail::SerializedPreset>& serializedPresets)
{
    std::vector<Preset<Struct> > presets;
    for (const auto& serialized : serializedPresets) {
        if (
            const auto preset
                =Deserialize<Struct>(serialized.settings, settingDescriptions)) {
            presets.push_back({ serialized.name, *preset });
        }
    }

    return presets;
}
} // namespace
} // namespace DynamicRangeProcessorUtils

std::vector<DynamicRangeProcessorUtils::CompressorPreset>
DynamicRangeProcessorUtils::GetCompressorPresets()
{
    return GetPresets<CompressorSettings>(
        compressorSettingDescriptions, Detail::serializedCompressorPresets);
}

std::vector<DynamicRangeProcessorUtils::LimiterPreset>
DynamicRangeProcessorUtils::GetLimiterPresets()
{
    return GetPresets<LimiterSettings>(
        limiterSettingDescriptions, Detail::serializedLimiterPresets);
}
