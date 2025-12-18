/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"
#include "types/ret.h"

#include "modularity/imoduleinterface.h"

namespace au::importexport {
class ILabelsImporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsImporter)

public:
    virtual ~ILabelsImporter() = default;

    virtual muse::Ret importData(const muse::io::path_t& filePath) = 0;
};
}
