/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <functional>
#include <vector>
#include <optional>

namespace au::toastnotification {
struct ToastNotificationAction {
    std::string text;
    std::function<void()> callback;
};

struct ToastNotificationItem {
    int id;
    int iconCode;
    std::string title;
    std::string message;
    std::vector<ToastNotificationAction> actions;
    bool dismissable;
    bool dismissed;
    std::optional<int> autoDismissTimeout;
};
}
