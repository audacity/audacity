#pragma once

#include "modularity/imoduleinterface.h"

#include "global/types/retval.h"

#include "processing/processingtypes.h"

namespace au::projectscene {
class ITracksGeometryController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITracksGeometryController)
public:
    virtual ~ITracksGeometryController() = default;

    virtual muse::ValCh<int> trackHeight(const processing::TrackId& trackId) const = 0;

    virtual void changeTrackHeight(const processing::TrackId& trackId, int deltaY) = 0;
};
}
