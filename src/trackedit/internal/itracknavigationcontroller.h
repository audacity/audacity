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

    virtual bool isNavigationEnabled() const = 0;
    virtual void setIsNavigationActive(bool active) = 0;
    virtual muse::async::Notification isNavigationActiveChanged() const = 0;

    virtual TrackId focusedTrack() const = 0;
    virtual void setFocusedTrack(const TrackId& trackId, bool highlight = false) = 0;
    virtual muse::async::Channel<TrackId, bool /*highlight*/> focusedTrackChanged() const = 0;

    virtual TrackItemKey focusedItem() const = 0;
    virtual void setFocusedItem(const TrackItemKey& key, bool highlight = false) = 0;
    virtual muse::async::Channel<TrackItemKey, bool /*highlight*/> focusedItemChanged() const = 0;

    virtual muse::async::Channel<TrackItemKey> openContextMenuRequested() const = 0;
};
}
