/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"

#include "modularity/imoduleinterface.h"

#include "labelstypes.h"

namespace au::importexport {
class ILabelsConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsConfiguration)

public:
    virtual ~ILabelsConfiguration() = default;

    virtual std::vector<FileFilter> fileFilter() const = 0;

    virtual muse::io::path_t labelsDirectoryPath() const = 0;
    virtual void setLabelsDirectoryPath(const muse::io::path_t& path) = 0;
};
}
