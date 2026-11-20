/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "framework/global/types/datetime.h"
#include "framework/global/types/val.h"
#include "framework/rcommand/commandtypes.h"

namespace au::macromanager {
class MacroId
{
public:
    explicit MacroId(std::string value)
        : m_value(std::move(value)) {}

    const std::string& toString() const { return m_value; }

    bool operator==(const MacroId&) const = default;

private:
    std::string m_value;
};

struct CommandStep {
    muse::rcommand::Command command;
    muse::rcommand::Params params;
};

struct MacroReference {
    std::string name;
};

struct CustomStep {
    std::string handlerId;
    muse::Val data;
};

using MacroStep = std::variant<CommandStep, MacroReference, CustomStep>;

struct MacroMetadata {
    muse::DateTime created;
    muse::DateTime edited;
    std::string audacityVersion;
};

struct Macro {
    std::string name;
    std::vector<MacroStep> steps;
    std::optional<MacroMetadata> metadata = std::nullopt;
};

using MacroList = std::vector<Macro>;
}
