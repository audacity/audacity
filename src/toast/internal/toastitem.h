/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/async/notification.h"
#include "framework/ui/view/iconcodes.h"
#include "framework/global/progress.h"
#include "framework/global/async/asyncable.h"

#include "toast/toasttypes.h"

namespace au::toast {
class ToastItem : public muse::async::Asyncable
{
public:
    ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
              std::chrono::seconds timeout = std::chrono::seconds(0), std::vector<ToastAction> actions = {});

    ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
              std::vector<ToastAction> actions = {}, std::shared_ptr<muse::Progress> progress = nullptr);
    ~ToastItem() = default;

    int id() const;
    std::string title() const;
    std::string message() const;
    muse::ui::IconCode::Code iconCode() const;
    bool isDismissible() const;
    const std::vector<ToastAction>& actions() const;

    int progress() const;
    muse::async::Notification progressChanged();

private:
    int m_id;
    std::string m_title;
    std::string m_message;
    muse::ui::IconCode::Code m_iconCode;
    bool m_dismissible;
    std::vector<ToastAction> m_actions;

    double m_currentProgress = 0;
    muse::async::Notification m_progressChanged;
    std::shared_ptr<muse::Progress> m_progress;
    std::map<int, std::unique_ptr<QTimer> > m_progressTimers;

    void stopTimer();
};
}
