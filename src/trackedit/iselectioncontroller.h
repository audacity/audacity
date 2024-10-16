#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/channel.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
//! NOTE Currently implemented in the au3wrap module
class ISelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(trackedit::ISelectionController)

public:
    virtual ~ISelectionController() = default;

    //! NOTE typical behavior
    //! * getter - getting the currently selected value (the selection itself may not be complete yet)
    //! * setter - setting the selected values ​​during selection,
    //! if the selection is completed, then it is necessary to pass: `complete = true`
    //! * valueChanged - notified when the selected value change (for example, the user moves the mouse cursor)
    //! * valueSelected - notified when value selection is complete

    // track selection
    virtual void resetSelectedTracks() = 0;
    virtual TrackIdList selectedTracks() const = 0;
    virtual void setSelectedTracks(const TrackIdList &trackIds, bool complete = true) = 0;
    virtual muse::async::Channel<TrackIdList> tracksSelected() const = 0;

    // clip selection
    virtual void resetSelectedClip() = 0;
    virtual ClipKey selectedClip() const = 0;
    virtual void setSelectedClip(const ClipKey& clipKey) = 0;
    virtual muse::async::Channel<ClipKey> clipSelected() const = 0;
    virtual double selectedClipStartTime() const = 0;
    virtual double selectedClipEndTime() const = 0;

    // data selection
    virtual void setSelectedTrackAudioData(trackedit::TrackId trackId) = 0;
    virtual void setSelectedClipAudioData(trackedit::TrackId trackId, secs_t time) = 0;
    virtual void resetDataSelection() = 0;
    virtual bool timeSelectionIsNotEmpty() const = 0;
    virtual bool isDataSelectedOnTrack(TrackId trackId) const = 0;

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
