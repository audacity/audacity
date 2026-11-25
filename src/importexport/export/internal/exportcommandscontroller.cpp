/*
* Audacity: A Digital Audio Editor
*/
#include "exportcommandscontroller.h"

#include <algorithm>

#include "framework/global/io/path.h"
#include "framework/global/progress.h"
#include "framework/global/log.h"

#include "trackedit/trackeditutils.h"

#include "../exportcommands.h"
#include "../types/exporttypes.h"
#include "exportcommandoptions.h"

using namespace au::importexport;
using namespace muse;

namespace {
ExportChannelsPref::ExportChannels toExportChannels(ExportChannelLayout layout, bool hasStereoTrack)
{
    switch (layout) {
    case ExportChannelLayout::Mono: return ExportChannelsPref::ExportChannels::MONO;
    case ExportChannelLayout::Stereo: return ExportChannelsPref::ExportChannels::STEREO;
    case ExportChannelLayout::Auto: break;
    }
    return hasStereoTrack ? ExportChannelsPref::ExportChannels::STEREO : ExportChannelsPref::ExportChannels::MONO;
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

template<typename Rate>
std::optional<int> sampleRateHz(Rate rate)
{
    return rate == Rate::ProjectRate ? std::nullopt : std::optional<int>(static_cast<int>(rate));
}
}

void ExportCommandsController::init()
{
    const auto cd = commandDispatcher();

    cd->onRequest<ExportWavCommand>(this, [this](const ExportWavCommand& c) {
        return exportAudio(WavSettings { c.encoding }, c.folder, sampleRateHz(c.sampleRate), c.channels, c.fileName);
    });
    cd->onRequest<ExportMp3Command>(this, [this](const ExportMp3Command& c) {
        return exportAudio(mp3Settings(c.quality), c.folder, sampleRateHz(c.sampleRate), c.channels, c.fileName);
    });
    cd->onRequest<ExportFlacCommand>(this, [this](const ExportFlacCommand& c) {
        return exportAudio(FlacSettings { c.bitDepth, c.level }, c.folder, sampleRateHz(c.sampleRate), c.channels, c.fileName);
    });
    cd->onRequest<ExportOggCommand>(this, [this](const ExportOggCommand& c) {
        return exportAudio(OggSettings { c.quality }, c.folder, sampleRateHz(c.sampleRate), c.channels, c.fileName);
    });
}

Ret ExportCommandsController::exportAudio(const ExportFormatSettings& settings, const std::string& folder,
                                          std::optional<int> sampleRate, ExportChannelLayout channels,
                                          const std::string& fileName)
{
    const int exportSampleRate = sampleRate ? *sampleRate : exporter()->defaultSampleRate(settings);

    const RetVal<ExportParameters> parameters = exporter()->exportParameters(settings, exportSampleRate);
    if (!parameters.ret) {
        return parameters.ret;
    }

    return runExport(std::string(extensionOf(settings)), exportSampleRate, channels, folder, fileName, toParametersVal(parameters.val));
}

Ret ExportCommandsController::runExport(const std::string& extension, int sampleRate, ExportChannelLayout channels,
                                        const std::string& folder, const std::string& fileName, const Val& parameters)
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported, "no project is open");
    }

    const std::optional<std::string> formatName = formatForExtension(extension);
    if (!formatName) {
        return make_ret(Ret::Code::NotSupported, "export format not available: " + extension);
    }

    OutputPathContext pathContext = macroOutputPathContext(exportConfiguration()->directoryPath(), project->path(),
                                                           project->displayName().toStdString());
    pathContext.macroFileName = macroRunner()->currentFileName();

    const RetVal<io::path_t> outputPath = exportOutputPath(folder, fileName, pathContext, extension);
    if (!outputPath.ret) {
        return outputPath.ret;
    }

    const Ret dirRet = fileSystem()->makePath(io::dirpath(outputPath.val));
    if (!dirRet) {
        return dirRet;
    }

    const ExportChannelsPref::ExportChannels channelsType
        = toExportChannels(channels, trackedit::utils::hasStereoTrack(globalContext()->currentTrackeditProject()->trackList()));

    IExporter::Options options {
        { IExporter::OptionKey::Format, Val(*formatName) },
        { IExporter::OptionKey::ProcessType, Val(static_cast<int>(ExportProcessType::FULL_PROJECT_AUDIO)) },
        { IExporter::OptionKey::ExportSampleRate, Val(sampleRate) },
        { IExporter::OptionKey::ExportChannelsType, Val(static_cast<int>(channelsType)) },
        { IExporter::OptionKey::Parameters, parameters },
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
