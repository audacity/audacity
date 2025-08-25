/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "types/exporttypes.h"
#include "types/ret.h"

namespace au::importexport {
class IExporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IExporter)

public:
    virtual ~IExporter() = default;

    virtual void init() = 0;
    virtual muse::Ret exportData(std::string filename) = 0;

    virtual std::vector<std::string> formatsList() const = 0;
    virtual int formatIndex(const std::string& format) const = 0;
    virtual std::string formatExtension(const std::string& format) const = 0;
    virtual bool isCustomFFmpegExportFormat() const = 0;

    virtual int maxChannels() const = 0;
    virtual std::vector<int> sampleRateList() const = 0;
    virtual int optionsCount() const = 0;

    virtual std::optional<ExportOption> option(int i) const = 0;
    virtual std::optional<OptionValue> value(int id) const = 0;
    virtual void setValue(int id, const OptionValue&) = 0;
};
}
