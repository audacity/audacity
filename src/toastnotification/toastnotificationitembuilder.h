/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "toastnotificationtypes.h"

namespace au::toastnotification {
class ToastNotificationItemBuilder
{
public:
    ToastNotificationItemBuilder();

    ToastNotificationItemBuilder& withIconCode(int iconCode);
    ToastNotificationItemBuilder& withTitle(const std::string& title);
    ToastNotificationItemBuilder& withMessage(const std::string& message);
    ToastNotificationItemBuilder& withActions(const std::vector<ToastNotificationAction>& actions);
    ToastNotificationItemBuilder& withDismissable(bool dismissable);
    ToastNotificationItemBuilder& withAutoDismissTimeout(int timeout);

    ToastNotificationItem build() const;

private:
    ToastNotificationItem m_item;
};
}
