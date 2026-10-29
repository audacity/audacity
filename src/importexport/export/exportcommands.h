/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string_view>

#include "framework/rcommand/commandtypes.h"

namespace au::importexport {
inline static const muse::rcommand::Command EXPORT_MP3_COMMAND("command://importexport/export/mp3");

constexpr std::string_view EXPORT_COMMAND_PATH_PREFIX = "importexport/export/";
inline static const std::string EXPORT_NAME_PARAM("name");

inline bool isExportCommand(const muse::rcommand::Command& command)
{
    return command.path().starts_with(EXPORT_COMMAND_PATH_PREFIX);
}
}
