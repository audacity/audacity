/*
* Audacity: A Digital Audio Editor
*/
#include "exportcommandscontroller.h"

#include <algorithm>

#include "framework/global/io/path.h"
#include "framework/global/progress.h"
#include "framework/global/log.h"

#include "../exportcommands.h"
#include "../types/exporttypes.h"

using namespace au::importexport;
using namespace muse;

namespace {
const std::string MACRO_OUTPUT_DIR("macro-output");
const std::string MP3_EXTENSION("mp3");
const std::string NAME_PARAM("name");
}

void ExportCommandsController::init()
{
    commandDispatcher()->onRequest(this, EXPORT_MP3_COMMAND, [this](const muse::rcommand::Params& params) {
        return exportToMacroOutput(MP3_EXTENSION, params);
    });
}

Ret ExportCommandsController::exportToMacroOutput(const std::string& extension, const muse::rcommand::Params& params)
{
    if (!globalContext()->currentProject()) {
        return make_ret(Ret::Code::NotSupported, "no project is open");
    }

    const std::optional<std::string> format = formatForExtension(extension);
    if (!format) {
        return make_ret(Ret::Code::NotSupported, "export format not available: " + extension);
    }

    const io::path_t outputDir = exportConfiguration()->directoryPath() + "/" + MACRO_OUTPUT_DIR;
    const Ret dirRet = fileSystem()->makePath(outputDir);
    if (!dirRet) {
        return dirRet;
    }

    const std::string baseName = params.contains(NAME_PARAM) ? params.at(NAME_PARAM).toString() : outputBaseName();
    const io::path_t outputPath = outputDir + "/" + baseName + "." + extension;

    const IExporter::Options options {
        { IExporter::OptionKey::Format, Val(*format) },
        { IExporter::OptionKey::ProcessType, Val(static_cast<int>(ExportProcessType::FULL_PROJECT_AUDIO)) },
    };

    LOGI() << "exporting to " << outputPath;

    return exporter()->exportData(outputPath, options, std::make_shared<Progress>());
}

std::optional<std::string> ExportCommandsController::formatForExtension(const std::string& extension) const
{
    for (const std::string& format : exporter()->formatsList()) {
        const std::vector<std::string> extensions = exporter()->formatExtensions(format);
        if (std::ranges::find(extensions, extension) != extensions.end()) {
            return format;
        }
    }

    return std::nullopt;
}

std::string ExportCommandsController::outputBaseName() const
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();

    const io::path_t path = project->path();
    if (!path.empty()) {
        return io::completeBasename(path).toStdString();
    }

    return project->displayName().toStdString();
}
