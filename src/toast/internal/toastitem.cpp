/*
* Audacity: A Digital Audio Editor
*/
#include "toastitem.h"

using namespace au::toast;

namespace {
int CURRENT_TOAST_ID = 1;
}

ToastItem::ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
                     std::chrono::seconds timeout, std::vector<ToastAction> actions)
    : m_id(CURRENT_TOAST_ID++),
    m_title(title),
    m_message(message),
    m_iconCode(iconCode),
    m_dismissible(dismissible),
    m_actions(actions)
{
    const int timeoutMs = std::chrono::duration_cast<std::chrono::milliseconds>(timeout).count();
    int interval = static_cast<int>(timeoutMs / 100);

    auto timer = std::make_unique<QTimer>();
    timer->setInterval(interval);
    timer->setSingleShot(false);
    timer->start();
    timer->callOnTimeout([this, timeoutMs, interval]() mutable {
        m_currentProgress += (static_cast<double>(interval) / static_cast<double>(timeoutMs)) * 100.0;
        m_progressChanged.notify();
        if (m_currentProgress >= 100.0) {
            stopTimer();
        }
    });

    m_progressTimers[m_id] = std::move(timer);
}

ToastItem::ToastItem(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
                     std::vector<ToastAction> actions, std::shared_ptr<muse::Progress> progress)
    : m_id(CURRENT_TOAST_ID++),
    m_title(title),
    m_message(message),
    m_iconCode(iconCode),
    m_dismissible(dismissible),
    m_actions(actions),
    m_progress(progress)
{
    if (m_progress) {
        m_progress->progressChanged().onReceive(this, [this](int64_t current, int64_t total, const std::string&) {
            if (total > 0) {
                m_currentProgress = static_cast<int>((static_cast<double>(current) / static_cast<double>(total)) * 100.0);
                m_progressChanged.notify();
            }
        });
    }
}

int ToastItem::id() const
{
    return m_id;
}

std::string ToastItem::title() const
{
    return m_title;
}

std::string ToastItem::message() const
{
    return m_message;
}

muse::ui::IconCode::Code ToastItem::iconCode() const
{
    return m_iconCode;
}

bool ToastItem::isDismissible() const
{
    return m_dismissible;
}

const std::vector<ToastAction>& ToastItem::actions() const
{
    return m_actions;
}

int ToastItem::progress() const
{
    return static_cast<int>(m_currentProgress);
}

muse::async::Notification ToastItem::progressChanged()
{
    return m_progressChanged;
}

void ToastItem::stopTimer()
{
    auto it = m_progressTimers.find(m_id);
    if (it != m_progressTimers.end()) {
        it->second->stop();
        m_progressTimers.erase(it);
    }
}
