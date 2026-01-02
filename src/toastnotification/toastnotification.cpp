/*
* Audacity: A Digital Audio Editor
*/
#include <vector>
#include <algorithm>

#include "framework/global/async/channel.h"

#include "toastnotificationtypes.h"
#include "toastnotification.h"

using namespace au::toastnotification;

namespace {
int CURRENT_ITEM_ID = 1;
}

std::vector<ToastNotificationItem> ToastNotification::currentNotificationList() const
{
    return m_notifications;
}

muse::async::Channel<ToastNotificationItem> ToastNotification::notificationAdded() const
{
    return m_notificationAdded;
}

muse::async::Channel<int> ToastNotification::notificationDismissed() const
{
    return m_notificationDismissed;
}

void ToastNotification::dismissNotification(int id)
{
    auto it = std::find_if(m_notifications.begin(), m_notifications.end(),
                           [id](const ToastNotificationItem& item) {
        return item.id == id;
    });

    if (it != m_notifications.end()) {
        (*it).dismissed = true;
        m_notificationDismissed.send(id);
    }
}

void ToastNotification::addNotification(const ToastNotificationItem& notification)
{
    ToastNotificationItem newNotification = notification;
    newNotification.id = CURRENT_ITEM_ID++;

    m_notifications.push_back(newNotification);
    m_notificationAdded.send(newNotification);
}
