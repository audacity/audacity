/*
* Audacity: A Digital Audio Editor
*/
#include "exportcommandoptions.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

using namespace au::importexport;
using namespace muse;

namespace {
const std::string PROJECT_DIR_VARIABLE("{ProjectDir}");

template<class ... Ts>
struct Overloaded : Ts ... {
    using Ts::operator() ...;
};

void replaceAll(std::string& text, const std::string& from, const std::string& to)
{
    for (size_t pos = text.find(from); pos != std::string::npos; pos = text.find(from, pos + to.size())) {
        text.replace(pos, from.size(), to);
    }
}

std::string optionLabelOrValue(const IExporter& exporter, const ExportOptionValue& value)
{
    const std::string rawValue = std::visit([](auto v) { return muse::rcommand::EnumContract<decltype(v)>::str_val(v); }, value);
    return exporter.optionLabel(value).value_or(rawValue);
}

std::string mp3QualitySummary(Mp3Quality quality, const IExporter& exporter)
{
    const auto prefixed = [&exporter](const char* pattern, const ExportOptionValue& value) {
        return mtrc("export", pattern).arg(String::fromStdString(optionLabelOrValue(exporter, value))).toStdString();
    };

    return std::visit(Overloaded {
            [&](const Mp3PresetSettings& s) { return prefixed(QT_TRANSLATE_NOOP("export", "Preset: %1"), s.preset); },
            [&](const Mp3VbrSettings& s) { return prefixed(QT_TRANSLATE_NOOP("export", "Variable: %1"), s.quality); },
            [&](const Mp3AbrSettings& s) { return prefixed(QT_TRANSLATE_NOOP("export", "Average: %1"), s.bitRate); },
            [&](const Mp3CbrSettings& s) { return prefixed(QT_TRANSLATE_NOOP("export", "Constant: %1"), s.bitRate); },
            [](const auto&) { return std::string(); },
        }, mp3Settings(quality));
}

template<typename C, typename Rate>
C commandWithOutput(const std::string& sampleRate, Rate defaultRate, ExportChannelLayout channels, const std::string& folder,
                    const std::string& fileName)
{
    C command;
    command.sampleRate = muse::rcommand::EnumContract<Rate>::enum_val(sampleRate).value_or(defaultRate);
    command.channels = channels;
    command.folder = folder;
    command.fileName = fileName;
    return command;
}

template<typename C>
RetVal<ExportFormatCommand> parsedCommand(const muse::rcommand::Params& params)
{
    C command;
    const muse::rcommand::ConvertParamsResult result = muse::rcommand::fromInputParams(params, command);
    if (!result.ret) {
        return RetVal<ExportFormatCommand>::make_ret(result.ret);
    }

    return RetVal<ExportFormatCommand>::make_ok(ExportFormatCommand(command));
}
}

ExportFormatSettings au::importexport::mp3Settings(Mp3Quality quality)
{
    const int value = static_cast<int>(quality);
    if (value >= MP3_CBR_BASE) {
        return Mp3CbrSettings { static_cast<Mp3BitRate>(value - MP3_CBR_BASE) };
    }
    if (value >= MP3_ABR_BASE) {
        return Mp3AbrSettings { static_cast<Mp3BitRate>(value - MP3_ABR_BASE) };
    }
    if (value >= MP3_VBR_BASE) {
        return Mp3VbrSettings { static_cast<Mp3VbrQuality>(value - MP3_VBR_BASE) };
    }
    return Mp3PresetSettings { static_cast<Mp3Preset>(value - MP3_PRESET_BASE) };
}

std::optional<Mp3Quality> au::importexport::mp3QualityOf(const ExportFormatSettings& settings)
{
    return std::visit(Overloaded {
        [](const Mp3PresetSettings& s) { return std::optional(static_cast<Mp3Quality>(MP3_PRESET_BASE + static_cast<int>(s.preset))); },
        [](const Mp3VbrSettings& s) { return std::optional(static_cast<Mp3Quality>(MP3_VBR_BASE + static_cast<int>(s.quality))); },
        [](const Mp3AbrSettings& s) { return std::optional(static_cast<Mp3Quality>(MP3_ABR_BASE + static_cast<int>(s.bitRate))); },
        [](const Mp3CbrSettings& s) { return std::optional(static_cast<Mp3Quality>(MP3_CBR_BASE + static_cast<int>(s.bitRate))); },
        [](const auto&) { return std::optional<Mp3Quality>(); },
    }, settings);
}

std::string_view au::importexport::extensionOf(const ExportFormatSettings& settings)
{
    return std::visit(Overloaded {
        [](const WavSettings&) { return std::string_view("wav"); },
        [](const Mp3PresetSettings&) { return std::string_view("mp3"); },
        [](const Mp3VbrSettings&) { return std::string_view("mp3"); },
        [](const Mp3AbrSettings&) { return std::string_view("mp3"); },
        [](const Mp3CbrSettings&) { return std::string_view("mp3"); },
        [](const FlacSettings&) { return std::string_view("flac"); },
        [](const OggSettings&) { return std::string_view("ogg"); },
    }, settings);
}

ExportFormatCommand au::importexport::formatCommand(const ExportFormatSettings& settings, const std::string& sampleRate,
                                                    ExportChannelLayout channels, const std::string& folder, const std::string& fileName)
{
    if (const std::optional<Mp3Quality> quality = mp3QualityOf(settings)) {
        auto command = commandWithOutput<ExportMp3Command>(sampleRate, Mp3SampleRateContract::def, channels, folder, fileName);
        command.quality = *quality;
        return command;
    }

    return std::visit(Overloaded {
        [&](const WavSettings& s) -> ExportFormatCommand {
            auto command = commandWithOutput<ExportWavCommand>(sampleRate, ExportSampleRateContract::def, channels, folder, fileName);
            command.encoding = s.encoding;
            return command;
        },
        [&](const FlacSettings& s) -> ExportFormatCommand {
            auto command = commandWithOutput<ExportFlacCommand>(sampleRate, ExportSampleRateContract::def, channels, folder, fileName);
            command.bitDepth = s.bitDepth;
            command.level = s.level;
            return command;
        },
        [&](const OggSettings& s) -> ExportFormatCommand {
            auto command = commandWithOutput<ExportOggCommand>(sampleRate, ExportSampleRateContract::def, channels, folder, fileName);
            command.quality = s.quality;
            return command;
        },
        [](const auto&) -> ExportFormatCommand { return ExportWavCommand {}; },
    }, settings);
}

ExportFormatSettings au::importexport::formatSettings(const ExportFormatCommand& command)
{
    return std::visit(Overloaded {
        [](const ExportWavCommand& c) -> ExportFormatSettings { return WavSettings { c.encoding }; },
        [](const ExportMp3Command& c) -> ExportFormatSettings { return mp3Settings(c.quality); },
        [](const ExportFlacCommand& c) -> ExportFormatSettings { return FlacSettings { c.bitDepth, c.level }; },
        [](const ExportOggCommand& c) -> ExportFormatSettings { return OggSettings { c.quality }; },
    }, command);
}

Val au::importexport::toVal(const ExportFormatCommand& command)
{
    muse::rcommand::Params params = std::visit([](const auto& c) { return muse::rcommand::toInputParams(c); }, command);
    params[EXPORT_FORMAT_KEY] = Val(std::string(extensionOf(formatSettings(command))));
    return Val(ValMap(params));
}

RetVal<ExportFormatCommand> au::importexport::formatCommandFromVal(const Val& val)
{
    muse::rcommand::Params params(val.toMap());
    const std::string format = params.at(EXPORT_FORMAT_KEY).toString();
    params.erase(EXPORT_FORMAT_KEY);

    if (format == "wav") {
        return parsedCommand<ExportWavCommand>(params);
    }
    if (format == "mp3") {
        return parsedCommand<ExportMp3Command>(params);
    }
    if (format == "flac") {
        return parsedCommand<ExportFlacCommand>(params);
    }
    if (format == "ogg") {
        return parsedCommand<ExportOggCommand>(params);
    }

    return RetVal<ExportFormatCommand>::make_ret(make_ret(Ret::Code::BadArgs,
                                                          qtrc("export", "Unknown export format: %1")
                                                          .arg(QString::fromStdString(format)).toStdString()));
}

std::string au::importexport::formatSummary(const ExportFormatCommand& command, const IExporter& exporter)
{
    return std::visit(Overloaded {
        [&](const ExportWavCommand& c) { return "WAV, " + optionLabelOrValue(exporter, c.encoding); },
        [&](const ExportMp3Command& c) { return "MP3, " + mp3QualitySummary(c.quality, exporter); },
        [&](const ExportFlacCommand& c) {
            return "FLAC, " + optionLabelOrValue(exporter, c.bitDepth) + ", " + optionLabelOrValue(exporter, c.level);
        },
        [&](const ExportOggCommand& c) { return "Ogg Vorbis, " + optionLabelOrValue(exporter, c.quality); },
    }, command);
}

OutputPathContext au::importexport::macroOutputPathContext(const io::path_t& exportDir, const io::path_t& projectPath,
                                                           const std::string& projectDisplayName)
{
    return OutputPathContext {
        exportDir + "/" + MACRO_OUTPUT_DIR,
        projectPath.empty() ? std::nullopt : std::optional<io::path_t>(io::dirpath(projectPath)),
        projectPath.empty() ? projectDisplayName : io::completeBasename(projectPath).toStdString()
    };
}

RetVal<io::path_t> au::importexport::exportOutputPath(const std::string& folder, const std::string& fileName,
                                                      const OutputPathContext& context, std::string_view extension)
{
    std::string dir = folder.empty() ? context.defaultDir.toStdString() : folder;
    if (dir.find(PROJECT_DIR_VARIABLE) != std::string::npos) {
        if (!context.projectDir) {
            LOGE() << "'folder' uses " << PROJECT_DIR_VARIABLE << " but the project is not saved";
            return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
        }
        replaceAll(dir, PROJECT_DIR_VARIABLE, context.projectDir->toStdString());
    }

    if (!io::isAbsolute(dir)) {
        LOGE() << "'folder' is not absolute: " << dir;
        return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
    }

    std::string name = fileName.empty() ? EXPORT_PROJECT_NAME_VARIABLE : fileName;
    if (context.macroFileName && name.find(EXPORT_PROJECT_NAME_VARIABLE) == std::string::npos) {
        return RetVal<io::path_t>::make_ret(make_ret(Ret::Code::BadArgs,
                                                     qtrc("export", "The export file name must contain %1 when the macro runs on files")
                                                     .arg(QString::fromStdString(EXPORT_PROJECT_NAME_VARIABLE)).toStdString()));
    }
    replaceAll(name, EXPORT_PROJECT_NAME_VARIABLE, context.macroFileName.value_or(context.projectName));
    name = io::escapeFileName(name).toStdString();
    if (name.empty() || name == "." || name == "..") {
        LOGE() << "invalid export file name: " << name;
        return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
    }

    if (io::suffix(name) != extension) {
        name += "." + std::string(extension);
    }

    return RetVal<io::path_t>::make_ok(io::path_t(dir) + "/" + name);
}
