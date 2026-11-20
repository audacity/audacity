/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/rcommand/commandtypes.h"

namespace au::macromanager {
muse::rcommand::Command makeMacroRunCommand(const std::string& macroName);
}
