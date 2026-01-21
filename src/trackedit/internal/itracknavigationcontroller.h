/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "actions/actiontypes.h"
#include "async/channel.h"

#include "global/modularity/imoduleinterface.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class ITrackNavigationController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackNavigationController);
public:
    virtual ~ITrackNavigationController() = default;

    virtual TrackId focusedTrack() const = 0;
    virtual void setFocusedTrack(TrackId trackId) = 0;
    virtual muse::async::Channel<TrackId> focusedTrackChanged() const = 0;

    virtual void focusTrackByIndex(const muse::actions::ActionData& args) = 0;
    virtual void focusPrevTrack() = 0;
    virtual void focusNextTrack() = 0;

    virtual void setFocusedItem(const TrackItemKey& key) = 0;
    virtual muse::async::Channel<TrackItemKey> focusedItemChanged() const = 0;

    virtual void trackRangeSelection() = 0;
    virtual void toggleSelectionOnFocusedTrack() = 0;
    virtual void multiSelectionUp() = 0;
    virtual void multiSelectionDown() = 0;

    virtual muse::async::Channel<TrackItemKey> openContextMenuRequested() const = 0;
};
}
