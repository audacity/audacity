/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "global/io/path.h"

#include "modularity/imoduleinterface.h"

namespace au::importexport {
class IImportConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IImportConfiguration)

public:

    virtual ~IImportConfiguration() = default;

    virtual muse::io::path_t labelsDirectoryPath() const = 0;
    virtual void setLabelsDirectoryPath(const muse::io::path_t& path) = 0;
};
}

