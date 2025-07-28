#pragma once

#include "io/path.h"
#include "modularity/imoduleinterface.h"

namespace au::importexport {
class IImporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IImporter)

public:
    virtual ~IImporter() = default;

    virtual void init() = 0;
    virtual bool import(const muse::io::path_t& filePath) = 0;
};
}
