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

    virtual void resetSelection() = 0;

    virtual muse::ValCh<std::vector<TrackId>> selectedTrackIds() const = 0;
    virtual void setSelectedTrackIds(const std::vector<TrackId>& trackIds) = 0;

    virtual muse::ValCh<secs_t> selectedStartTime() const = 0;
    virtual void setSelectedStartTime(const secs_t time) = 0;
    virtual muse::ValCh<secs_t> selectedEndTime() const = 0;
    virtual void setSelectedEndTime(const secs_t time) = 0;
};
}
