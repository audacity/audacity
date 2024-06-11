#pragma once

#include "modularity/imoduleinterface.h"

#include "global/types/retval.h"

#include "processing/processingtypes.h"

namespace au::processing {
//! NOTE Currently implemented in the au3wrap module
class IProcessingSelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProcessingSelectionController)

public:
    virtual ~IProcessingSelectionController() = default;

    // data selection
    virtual void resetDataSelection() = 0;
    virtual muse::ValCh<std::vector<TrackId>> dataSelectedOnTracks() const = 0;
    virtual void setDataSelectedOnTracks(const std::vector<TrackId>& trackIds) = 0;
    virtual muse::ValCh<secs_t> dataSelectedStartTime() const = 0;
    virtual void setDataSelectedStartTime(const secs_t time) = 0;
    virtual muse::ValCh<secs_t> dataSelectedEndTime() const = 0;
    virtual void setDataSelectedEndTime(const secs_t time) = 0;
};
}
