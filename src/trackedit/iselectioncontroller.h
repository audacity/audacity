#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/channel.h"

#include "trackedit/trackedittypes.h"

#include <optional>

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
    virtual void setSelectedTracks(const TrackIdList& trackIds, bool complete = true) = 0;
    virtual void addSelectedTrack(const trackedit::TrackId& trackId) = 0;
    virtual muse::async::Channel<TrackIdList> tracksSelected() const = 0;
    virtual std::optional<trackedit::TrackId> determinePointedTrack(double y) const = 0;
    virtual trackedit::TrackIdList determinateTracks(double y1, double y2) const = 0;

    // clip selection
    virtual void resetSelectedClips() = 0;
    virtual bool hasSelectedClips() const = 0;
    virtual ClipKeyList selectedClips() const = 0;
    virtual ClipKeyList selectedClipsInTrackOrder() const = 0;
    virtual void setSelectedClips(const ClipKeyList& clipKeys, bool complete = true) = 0;
    virtual std::optional<ClipId> setSelectedClip(trackedit::TrackId trackId, secs_t time) = 0;
    virtual void addSelectedClip(const ClipKey& clipKey) = 0;
    virtual void removeClipSelection(const ClipKey& clipKey) = 0;
    virtual muse::async::Channel<ClipKeyList> clipsSelected() const = 0;

    //! NOTE: for now it's used only for ruler
    //! hightlight when single clip is selected
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

    virtual trackedit::secs_t selectionStartTime() const = 0;
    virtual void setSelectionStartTime(trackedit::secs_t time) = 0;

    // grouping
    virtual bool selectionContainsGroup() const = 0;
    virtual bool isSelectionGrouped() const = 0;

    virtual void resetTimeSelection() = 0;
};
}
