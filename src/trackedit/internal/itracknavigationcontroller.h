/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "async/channel.h"
#include "async/notification.h"

#include "global/modularity/imoduleinterface.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class ITrackNavigationController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackNavigationController);
public:
    virtual ~ITrackNavigationController() = default;

    virtual TrackId focusedTrack() const = 0;
    virtual void setFocusedTrack(const TrackId& trackId) = 0;
    virtual muse::async::Notification focusedTrackChanged() const = 0;

    virtual TrackItemKey focusedItem() const = 0;
    virtual void setFocusedItem(const TrackItemKey& key) = 0;
    virtual muse::async::Notification focusedItemChanged() const = 0;

    virtual void trackRangeSelection() = 0;
    virtual void toggleSelectionOnFocusedTrack() = 0;
    virtual void multiSelectionUp() = 0;
    virtual void multiSelectionDown() = 0;

    virtual muse::async::Channel<TrackItemKey> openContextMenuRequested() const = 0;
};
}
