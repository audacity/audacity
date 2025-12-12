#pragma once

#include "io/path.h"
#include "modularity/imoduleinterface.h"

#include "types/importtypes.h"
#include "trackedit/trackedittypes.h"

namespace au::importexport {
class IImporter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IImporter)

public:
    virtual ~IImporter() = default;

    virtual void init() = 0;
    virtual FileInfo fileInfo(const muse::io::path_t& filePath) = 0;
    virtual bool import(const muse::io::path_t& filePath) = 0;
    virtual bool importIntoTrack(const muse::io::path_t& filePath, trackedit::TrackId dstTrackId, trackedit::secs_t startTime) = 0;
};
}
