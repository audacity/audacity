/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>
#include <string_view>

#include "framework/global/io/path.h"
#include "framework/global/types/retval.h"

#include "../exportcommands.h"
#include "../types/exportformatsettings.h"

namespace au::importexport {
struct OutputPathContext {
    muse::io::path_t defaultDir;
    std::optional<muse::io::path_t> projectDir;
    std::string name;
};

ExportFormatSettings defaultFormatSettings(ExportAudioFormat format);
std::string_view extensionOf(const ExportFormatSettings& settings);

muse::RetVal<muse::io::path_t> exportOutputPath(const std::string& path, const OutputPathContext& context, std::string_view extension);
}
