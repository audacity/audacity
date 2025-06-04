/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

namespace au::importexport {
class IExporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IExporter)

public:
    virtual ~IExporter() = default;

    virtual bool doExport() = 0;

    virtual std::vector<std::string> formatList() const = 0;
    virtual int formatIndex(const std::string& format) const = 0;
    virtual std::string formatExtension(const std::string& format) const = 0;
};
}
