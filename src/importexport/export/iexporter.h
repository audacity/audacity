/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <tuple>
#include <vector>

#include "framework/global/io/path.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/progress.h"
#include "framework/global/types/val.h"

#include "types/exporttypes.h"
#include "types/ret.h"

namespace au::importexport {
struct ExportDataConfig {
    std::string format;
    ExportProcessType processType = ExportProcessType::FULL_PROJECT_AUDIO;
    int exportChannelsType = static_cast<int>(ExportChannelsPref::ExportChannels::STEREO);
    int exportChannels = 2;
    muse::Val exportCustomChannelMapping;
    int exportSampleRate = 44100;
};

using ExportParameters = std::vector<std::tuple<int, OptionValue> >;

class IExporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IExporter)

public:
    virtual ~IExporter() = default;

    virtual void init() = 0;
    virtual muse::Ret exportData(const muse::io::path_t& path, muse::ProgressPtr progress = nullptr) = 0;
    virtual muse::Ret exportData(const muse::io::path_t& path, const ExportDataConfig& config, const ExportParameters& parameters,
                                 muse::ProgressPtr progress = nullptr) = 0;

    virtual std::vector<std::string> formatsList() const = 0;
    virtual int formatIndex(const std::string& format) const = 0;
    virtual std::vector<std::string> formatExtensions(const std::string& format) const = 0;
    virtual std::vector<std::string> cloudPreferredAudioFormats() const = 0;
    virtual ExportParameters cloudExportParameters(const std::string& format) const = 0;
    virtual bool isCustomFFmpegExportFormat() const = 0;
    virtual bool isOggExportFormat() const = 0;
    virtual bool hasMetadata() const = 0;

    virtual int maxChannels() const = 0;
    virtual std::vector<int> sampleRateList() const = 0;
    virtual int optionsCount() const = 0;

    virtual std::optional<ExportOption> option(int i) const = 0;
    virtual std::optional<OptionValue> value(int id) const = 0;
    virtual void setValue(int id, const OptionValue&) = 0;
};
}
