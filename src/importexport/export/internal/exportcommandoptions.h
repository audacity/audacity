/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "framework/global/io/path.h"
#include "framework/global/types/retval.h"
#include "framework/global/types/val.h"

#include "../exportcommands.h"
#include "../iexporter.h"
#include "../types/exportformatsettings.h"

namespace au::importexport {
inline static const std::string MACRO_OUTPUT_DIR("macro-output");

struct OutputPathContext {
    muse::io::path_t defaultDir;
    std::optional<muse::io::path_t> projectDir;
    std::string projectName;
    std::optional<std::string> macroFileName;
};

using ExportFormatCommand = std::variant<ExportWavCommand, ExportMp3Command, ExportFlacCommand, ExportOggCommand>;

inline static const std::string EXPORT_FORMAT_KEY("format");

ExportFormatSettings mp3Settings(Mp3Quality quality);
std::optional<Mp3Quality> mp3QualityOf(const ExportFormatSettings& settings);
std::string_view extensionOf(const ExportFormatSettings& settings);

ExportFormatCommand formatCommand(const ExportFormatSettings& settings, const std::string& sampleRate, ExportChannelLayout channels,
                                  const std::string& folder, const std::string& fileName);
ExportFormatSettings formatSettings(const ExportFormatCommand& command);
muse::Val toVal(const ExportFormatCommand& command);
muse::RetVal<ExportFormatCommand> formatCommandFromVal(const muse::Val& val);

std::string formatSummary(const ExportFormatCommand& command, const IExporter& exporter);

OutputPathContext macroOutputPathContext(const muse::io::path_t& exportDir, const muse::io::path_t& projectPath,
                                         const std::string& projectDisplayName);
muse::RetVal<muse::io::path_t> exportOutputPath(const std::string& folder, const std::string& fileName, const OutputPathContext& context,
                                                std::string_view extension);
}
