#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/channel.h"

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

    //! NOTE typical behavior
    //! * getter - getting the currently selected value (the selection itself may not be complete yet)
    //! * setter - setting the selected values ​​during selection,
    //! if the selection is completed, then it is necessary to pass: `complete = true`
    //! * valueChanged - notified when the selected value change (for example, the user moves the mouse cursor)
    //! * valueSelected - notified when value selection is complete

    virtual std::vector<TrackId> dataSelectedOnTracks() const = 0;
    virtual void setDataSelectedOnTracks(const std::vector<TrackId>& trackIds, bool complete) = 0;
    virtual muse::async::Channel<std::vector<TrackId>> dataSelectedOnTracksChanged() const = 0;
    virtual muse::async::Channel<std::vector<TrackId>> dataSelectedOnTracksSelected() const = 0;

    virtual secs_t dataSelectedStartTime() const = 0;
    virtual void setDataSelectedStartTime(secs_t time, bool complete) = 0;
    virtual muse::async::Channel<secs_t> dataSelectedStartTimeChanged() const = 0;
    virtual muse::async::Channel<secs_t> dataSelectedStartTimeSelected() const = 0;

    virtual secs_t dataSelectedEndTime() const = 0;
    virtual void setDataSelectedEndTime(secs_t time, bool complete) = 0;
    virtual muse::async::Channel<secs_t> dataSelectedEndTimeChanged() const = 0;
    virtual muse::async::Channel<secs_t> dataSelectedEndTimeSelected() const = 0;
};
}
