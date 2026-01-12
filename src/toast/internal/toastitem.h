/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/async/notification.h"
#include "framework/ui/view/iconcodes.h"
#include "framework/global/progress.h"

#include "toast/toasttypes.h"

namespace au::toast {
class ToastItem
{
public:
    ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
              std::chrono::seconds timeout = std::chrono::seconds(0), std::vector<ToastAction> actions = {});

    ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
              std::vector<ToastAction> actions = {}, std::shared_ptr<muse::Progress> progress = nullptr, bool showProgressInfo = false);
    ~ToastItem() = default;

    int id() const;
    std::string title() const;
    std::string message() const;
    muse::ui::IconCode::Code iconCode() const;
    bool isDismissible() const;
    const std::vector<ToastAction>& actions() const;
    std::chrono::seconds timeout() const;

    double currentProgress() const;
    void setCurrentProgress(double progress);
    muse::async::Notification progressChanged() const;
    std::shared_ptr<muse::Progress> progress() const;
    bool showProgressInfo() const;
    int timeElapsed() const;

private:
    int m_id = 0;
    std::string m_title = "Title";
    std::string m_message = "Message";
    muse::ui::IconCode::Code m_iconCode = muse::ui::IconCode::Code::NONE;
    bool m_dismissible = true;
    std::chrono::seconds m_timeout = std::chrono::seconds(0);
    std::vector<ToastAction> m_actions;

    double m_currentProgress = 0;
    muse::async::Notification m_progressChanged;
    std::shared_ptr<muse::Progress> m_progress;
    bool m_showProgressInfo = false;

    std::chrono::steady_clock::time_point m_creationTime = std::chrono::steady_clock::now();
};
}
