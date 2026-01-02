/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"

#include "itoastnotification.h"

namespace au::toastnotification {
class ToastNotification : public IToastNotification, public muse::async::Asyncable
{
public:
    std::vector<ToastNotificationItem> currentNotificationList() const override;

    muse::async::Channel<ToastNotificationItem> notificationAdded() const override;
    muse::async::Channel<int> notificationDismissed() const override;

    void dismissNotification(int id) override;
    void addNotification(const ToastNotificationItem& notification) override;

private:
    std::vector<ToastNotificationItem> m_notifications;

    muse::async::Channel<ToastNotificationItem> m_notificationAdded;
    muse::async::Channel<int> m_notificationDismissed;
};
}
