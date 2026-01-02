/*
* Audacity: A Digital Audio Editor
*/
#include "toastnotificationitembuilder.h"

using namespace au::toastnotification;

ToastNotificationItemBuilder::ToastNotificationItemBuilder()
{
    m_item.id = 0;
    m_item.iconCode = 0;
    m_item.title = "";
    m_item.message = "";
    m_item.dismissable = true;
    m_item.dismissed = false;
    m_item.autoDismissTimeout = std::nullopt;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withIconCode(int iconCode)
{
    m_item.iconCode = iconCode;
    return *this;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withTitle(const std::string& title)
{
    m_item.title = title;
    return *this;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withMessage(const std::string& message)
{
    m_item.message = message;
    return *this;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withActions(const std::vector<ToastNotificationAction>& actions)
{
    m_item.actions = actions;
    return *this;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withDismissable(bool dismissable)
{
    m_item.dismissable = dismissable;
    return *this;
}

ToastNotificationItemBuilder& ToastNotificationItemBuilder::withAutoDismissTimeout(int timeout)
{
    m_item.autoDismissTimeout = timeout;
    return *this;
}

ToastNotificationItem ToastNotificationItemBuilder::build() const
{
    return m_item;
}
