/*
* Audacity: A Digital Audio Editor
*/
#include "exportcommandoptions.h"

#include "framework/global/log.h"

using namespace au::importexport;
using namespace muse;

namespace {
const std::string PROJECT_DIR_VARIABLE("{ProjectDir}");
const std::string PROJECT_NAME_VARIABLE("{ProjectName}");

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
}

ExportFormatSettings au::importexport::defaultFormatSettings(ExportAudioFormat format)
{
    switch (format) {
    case ExportAudioFormat::Wav: return WavSettings {};
    case ExportAudioFormat::Mp3: return Mp3PresetSettings {};
    case ExportAudioFormat::Flac: return FlacSettings {};
    case ExportAudioFormat::Ogg: return OggSettings {};
    }
    return WavSettings {};
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

RetVal<io::path_t> au::importexport::exportOutputPath(const std::string& outputPath, const OutputPathContext& context,
                                                      std::string_view extension)
{
    const std::string name = io::escapeFileName(context.name).toStdString();
    if (name.empty() || name == "." || name == "..") {
        LOGE() << "invalid export name: " << context.name;
        return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
    }

    const std::string suffix = "." + std::string(extension);
    if (outputPath.empty()) {
        return RetVal<io::path_t>::make_ok(context.defaultDir + "/" + name + suffix);
    }

    std::string path = outputPath;
    if (path.find(PROJECT_DIR_VARIABLE) != std::string::npos) {
        if (!context.projectDir) {
            LOGE() << "'path' uses " << PROJECT_DIR_VARIABLE << " but the project is not saved";
            return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
        }
        replaceAll(path, PROJECT_DIR_VARIABLE, context.projectDir->toStdString());
    }
    replaceAll(path, PROJECT_NAME_VARIABLE, name);

    if (!io::isAbsolute(path)) {
        LOGE() << "'path' is not absolute: " << path;
        return RetVal<io::path_t>::make_ret(Ret::Code::BadArgs);
    }

    if (io::suffix(path) != extension) {
        path += suffix;
    }

    return RetVal<io::path_t>::make_ok(io::path_t(path));
}
