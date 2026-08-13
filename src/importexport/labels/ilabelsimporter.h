/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "io/path.h"
#include "types/retval.h"

#include "modularity/imoduleinterface.h"

#include "trackedit/trackedittypes.h"

namespace au::importexport {
class ILabelsImporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsImporter)

public:
    virtual ~ILabelsImporter() = default;

    //! NOTE: returns the id of the label track created for the imported labels
    virtual muse::RetVal<trackedit::TrackId> importData(const muse::io::path_t& filePath) = 0;

    virtual std::vector<std::string> supportedExtensions() const = 0;
};
}
