/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "rcommand/commandtypes.h"

namespace au::macromanager {
struct MacroStep {
    muse::rcommand::Command command;
    muse::rcommand::Params params;
};

struct Macro {
    std::string name;
    std::vector<MacroStep> steps;
    bool builtin = false;
};

using MacroList = std::vector<Macro>;
}
