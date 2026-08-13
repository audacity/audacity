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

    //! NOTE: imports into the given label track if valid, otherwise creates a new one;
    //! returns the id of the label track holding the imported labels
    virtual muse::RetVal<trackedit::TrackId> importData(const muse::io::path_t& filePath, trackedit::TrackId dstTrackId = -1) = 0;

    virtual std::vector<std::string> supportedExtensions() const = 0;
};
}
