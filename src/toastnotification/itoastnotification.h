/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"

#include "toastnotificationtypes.h"

namespace au::toastnotification {
class IToastNotification : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IToastNotification)
public:
    virtual ~IToastNotification() = default;

    virtual std::vector<ToastNotificationItem> currentNotificationList() const = 0;

    virtual muse::async::Channel<ToastNotificationItem> notificationAdded() const = 0;
    virtual muse::async::Channel<int> notificationDismissed() const = 0;

    virtual void dismissNotification(int) = 0;
    virtual void addNotification(const ToastNotificationItem&) = 0;
};
}
