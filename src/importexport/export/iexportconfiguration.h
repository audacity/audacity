/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "global/async/channel.h"
#include "global/io/path.h"

#include "modularity/imoduleinterface.h"

#include "types/exporttypes.h"

namespace au::importexport {
class IExportConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IExportConfiguration)

public:

    virtual ~IExportConfiguration() = default;

    virtual ProcessType process() const = 0;
    virtual void setProcess(ProcessType process) = 0;
    virtual muse::async::Notification processChanged() const = 0;

    virtual std::string filename() const = 0;
    virtual void setFilename(const std::string& filename) = 0;
    virtual muse::async::Notification filenameChanged() const = 0;

    virtual muse::io::path_t directoryPath() const = 0;
    virtual void setDirectoryPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Notification directoryPathChanged() const = 0;

    virtual std::string currentFormat() const = 0;
    virtual void setCurrentFormat(const std::string& format) = 0;
    virtual muse::async::Notification currentFormatChanged() const = 0;

    virtual ExportChannelsPref::ExportChannels exportChannels() const = 0;
    virtual void setExportChannels(ExportChannelsPref::ExportChannels channels) = 0;
    virtual muse::async::Notification exportChannelsChanged() const = 0;

    virtual uint64_t exportSampleRate() const = 0;
    virtual void setExportSampleRate(uint64_t newRate) = 0;
    virtual muse::async::Notification exportSampleRateChanged() const = 0;

    virtual std::vector<std::string> exportSampleFormatList() const = 0;
    virtual std::string exportSampleFormat() const = 0;
    virtual void setExportSampleFormat(const std::string& format) = 0;
    virtual muse::async::Notification exportSampleFormatChanged() const = 0;
};
}
