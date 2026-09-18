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
    virtual muse::async::Channel<TrackId, bool /*highlight*/> focusedTrackChanged() const = 0;

    virtual TrackFocus focus() const = 0;
    virtual void setFocus(const TrackFocus& focus, bool highlight = false) = 0;
    virtual muse::async::Channel<TrackFocus, bool /*highlight*/> focusChanged() const = 0;

    virtual TrackItemKeyList itemKeysInRange(const TrackItemKey& anchor, const TrackItemKey& target) const = 0;

    virtual void resetNavigation() = 0;

    virtual muse::async::Channel<TrackItemKey> openContextMenuRequested() const = 0;
    virtual muse::async::Channel<TrackId> openRulerContextMenuRequested() const = 0;
};
}
