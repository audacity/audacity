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
#include "exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;

namespace {
const std::string MACRO_OUTPUT_DIR("macro-output");

ExportChannelsPref::ExportChannels toExportChannels(ExportChannelLayout layout)
{
    return layout == ExportChannelLayout::Mono ? ExportChannelsPref::ExportChannels::MONO : ExportChannelsPref::ExportChannels::STEREO;
}

Val toParametersVal(const ExportParameters& parameters)
{
    ValList list;
    for (const auto& [id, value] : parameters) {
        list.push_back(Val(ValMap {
                { "id", Val(id) },
                { "value", std::visit([](const auto& v) { return Val(v); }, value) },
            }));
    }
    return Val(list);
}
}

void ExportCommandsController::init()
{
    const auto cd = commandDispatcher();

    cd->onRequest<ExportAudioCommand>(this, [this](const ExportAudioCommand& c) {
        return exportAudio(defaultFormatSettings(c.format), c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportWavCommand>(this, [this](const ExportWavCommand& c) {
        return exportAudio(WavSettings { c.encoding }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportMp3PresetCommand>(this, [this](const ExportMp3PresetCommand& c) {
        return exportAudio(Mp3PresetSettings { c.quality }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportMp3VbrCommand>(this, [this](const ExportMp3VbrCommand& c) {
        return exportAudio(Mp3VbrSettings { c.quality }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportMp3AbrCommand>(this, [this](const ExportMp3AbrCommand& c) {
        return exportAudio(Mp3AbrSettings { c.bitRate }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportMp3CbrCommand>(this, [this](const ExportMp3CbrCommand& c) {
        return exportAudio(Mp3CbrSettings { c.bitRate }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportFlacCommand>(this, [this](const ExportFlacCommand& c) {
        return exportAudio(FlacSettings { c.bitDepth, c.level }, c.path, c.sampleRate, c.channels, c.name);
    });
    cd->onRequest<ExportOggCommand>(this, [this](const ExportOggCommand& c) {
        return exportAudio(OggSettings { c.quality }, c.path, c.sampleRate, c.channels, c.name);
    });
}

Ret ExportCommandsController::exportAudio(const ExportFormatSettings& settings, const std::string& path, int sampleRate,
                                          ExportChannelLayout channels, const std::string& name)
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported, "no project is open");
    }

    const std::string extension(extensionOf(settings));
    const std::optional<std::string> formatName = formatForExtension(extension);
    if (!formatName) {
        return make_ret(Ret::Code::NotSupported, "export format not available: " + extension);
    }

    const int exportSampleRate = sampleRate > 0 ? sampleRate : exporter()->projectSampleRate();

    const RetVal<ExportParameters> parameters = exporter()->exportParameters(settings, exportSampleRate);
    if (!parameters.ret) {
        return parameters.ret;
    }

    const io::path_t projectPath = project->path();
    const OutputPathContext pathContext {
        exportConfiguration()->directoryPath() + "/" + MACRO_OUTPUT_DIR,
        projectPath.empty() ? std::nullopt : std::optional<io::path_t>(io::dirpath(projectPath)),
        name.empty() ? outputBaseName() : name
    };

    const RetVal<io::path_t> outputPath = exportOutputPath(path, pathContext, extension);
    if (!outputPath.ret) {
        return outputPath.ret;
    }

    const Ret dirRet = fileSystem()->makePath(io::dirpath(outputPath.val));
    if (!dirRet) {
        return dirRet;
    }

    const IExporter::Options options {
        { IExporter::OptionKey::Format, Val(*formatName) },
        { IExporter::OptionKey::ProcessType, Val(static_cast<int>(ExportProcessType::FULL_PROJECT_AUDIO)) },
        { IExporter::OptionKey::ExportSampleRate, Val(exportSampleRate) },
        { IExporter::OptionKey::ExportChannelsType, Val(static_cast<int>(toExportChannels(channels))) },
        { IExporter::OptionKey::Parameters, toParametersVal(parameters.val) },
    };

    LOGI() << "exporting to " << outputPath.val;

    return exporter()->exportData(outputPath.val, options, std::make_shared<Progress>());
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
