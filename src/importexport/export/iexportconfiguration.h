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

    virtual ExportProcessType processType() const = 0;
    virtual void setProcessType(ExportProcessType process) = 0;
    virtual muse::async::Notification processTypeChanged() const = 0;

    virtual muse::io::path_t directoryPath() const = 0;
    virtual void setDirectoryPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Notification directoryPathChanged() const = 0;

    virtual std::string currentFormat() const = 0;
    virtual void setCurrentFormat(const std::string& format) = 0;
    virtual muse::async::Notification currentFormatChanged() const = 0;

    virtual int exportChannels() const = 0;
    virtual void setExportChannels(int channels) = 0;
    virtual muse::async::Notification exportChannelsChanged() const = 0;

    virtual int exportSampleRate() const = 0;
    virtual void setExportSampleRate(int newRate) = 0;
    virtual muse::async::Notification exportSampleRateChanged() const = 0;

    virtual std::vector<std::string> exportSampleFormatList() const = 0;
    virtual std::string exportSampleFormat() const = 0;
    virtual void setExportSampleFormat(const std::string& format) = 0;
    virtual muse::async::Notification exportSampleFormatChanged() const = 0;
};
}
