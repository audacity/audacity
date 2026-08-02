/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "effects/effects_base/effectstypes.h"
#include "framework/extensions/extensionstypes.h"

namespace au::effects::extensions {
enum class ParameterType {
    Boolean,
    Int64,
    Double,
    String,
    Enumeration,
    File,
    Directory,
};

using Value = std::variant<bool, int64_t, double, std::string>;

struct EnumChoice {
    std::string token;
    std::string name;
};

struct ParameterDescriptor {
    std::string id;
    std::string name;
    std::string description;
    std::string unit;
    ParameterType type = ParameterType::Boolean;
    Value defaultValue;
    std::optional<Value> minimum;
    std::optional<Value> maximum;
    std::optional<Value> step;
    std::vector<EnumChoice> choices;
};

struct EffectDescriptor {
    std::string extensionId;
    std::string effectId;
    std::string title;
    std::string description;
    std::string vendor;
    std::string version;
    std::string group;
    muse::io::path_t scriptPath;
    std::string factory = "createEffect";
    muse::extensions::Manifest manifest;
};

inline EffectType effectTypeFromGroup(const std::string& group)
{
    if (group == "generate") {
        return EffectType::Generator;
    }
    if (group == "analyze") {
        return EffectType::Analyzer;
    }
    if (group == "tools") {
        return EffectType::Tool;
    }
    return EffectType::Processor;
}

struct Settings {
    std::map<std::string, Value> values;
};
} // namespace au::effects::extensions
