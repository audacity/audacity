/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"
#include "types/ret.h"

#include "modularity/imoduleinterface.h"

#include "trackedit/trackedittypes.h"

namespace au::importexport {
class ILabelsExporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsExporter)

public:
    virtual ~ILabelsExporter() = default;

    virtual muse::Ret exportData(const muse::io::path_t& filePath, const trackedit::TrackIdList& includedLabelTracksIds = {}) = 0;

    virtual std::vector<std::string> fileFilter() = 0;
};
}
