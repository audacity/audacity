/*
* Audacity: A Digital Audio Editor
*/
#include "toastprovider.h"

using namespace au::toast;

muse::async::Promise<ToastActionCode> ToastProvider::show(ToastItem item)
{
    return muse::async::make_promise<ToastActionCode>([this, item](auto resolve, auto) {
        int id = item.id();
        m_toasts.emplace_back(std::make_shared<ToastItem>(item));
        m_toastAdded.send(m_toasts.back());

        m_resolvers[id] = std::move(resolve);

        checkProgress(id);
        checkTimer(id);

        return muse::async::Promise<ToastActionCode>::dummy_result();
    }, muse::async::PromiseType::AsyncByBody);
}

muse::async::Channel<std::shared_ptr<ToastItem> > ToastProvider::toastAdded() const
{
    return m_toastAdded;
}

muse::async::Channel<int> ToastProvider::toastDismissed() const
{
    return m_toastDismissed;
}

void ToastProvider::dismissToast(int id)
{
    for (int i = 0; i < static_cast<int>(m_toasts.size()); ++i) {
        if (m_toasts.at(i)->id() == id) {
            m_toastDismissed.send(id);
            const auto& item = m_toasts.at(i);
            cleanup(item->id());
            m_toasts.erase(m_toasts.begin() + i);
            return;
        }
    }
}

void ToastProvider::executeAction(int id, ToastActionCode actionCode)
{
    resolveToast(id, actionCode);
    dismissToast(id);
}

void ToastProvider::cleanup(int id)
{
    auto timerIt = m_progressTimers.find(id);
    if (timerIt != m_progressTimers.end()) {
        m_progressTimers.erase(timerIt);
    }

    auto progressIt = m_progresses.find(id);
    if (progressIt != m_progresses.end()) {
        m_progresses.erase(progressIt);
    }

    auto resolverIt = m_resolvers.find(id);
    if (resolverIt != m_resolvers.end()) {
        resolveToast(id, ToastActionCode::None);
    }
}

void ToastProvider::checkProgress(int id)
{
    const auto itemIt = std::find_if(m_toasts.begin(), m_toasts.end(), [id](const std::shared_ptr<ToastItem>& toast) {
        return toast->id() == id;
    });

    if (itemIt == m_toasts.end()) {
        return;
    }

    auto& item = *itemIt;
    auto progress = item->progress();
    if (progress) {
        progress->progressChanged().onReceive(this, [this, item](int64_t current, int64_t total, const std::string&) {
            if (total > 0) {
                const double newProgress = static_cast<int>((static_cast<double>(current) / static_cast<double>(total)) * 100.0);
                item->setCurrentProgress(newProgress);

                if (newProgress >= 100.0) {
                    dismissToast(item->id());
                }
            }
        });
    }
}

void ToastProvider::checkTimer(int id)
{
    const auto itemIt = std::find_if(m_toasts.begin(), m_toasts.end(), [id](const std::shared_ptr<ToastItem>& toast) {
        return toast->id() == id;
    });

    if (itemIt == m_toasts.end()) {
        return;
    }

    auto& item = *itemIt;
    if (item->timeout().count() > 0) {
        const int timeoutMs = std::chrono::duration_cast<std::chrono::milliseconds>(item->timeout()).count();

        if (timeoutMs > 0) {
            int interval = static_cast<int>(timeoutMs / 100);
            auto timer = std::make_unique<QTimer>();
            timer->setInterval(interval);
            timer->setSingleShot(false);
            timer->start();
            timer->callOnTimeout([this, item, timeoutMs, interval]() {
                double currentProgress = item->currentProgress() + ((static_cast<double>(interval) / static_cast<double>(timeoutMs)) * 100.0);
                item->setCurrentProgress(currentProgress);
                if (currentProgress >= 100.0) {
                    dismissToast(item->id());
                }
            });

            m_progressTimers[item->id()] = std::move(timer);
        }
    }
}

void ToastProvider::resolveToast(int id, ToastActionCode actionCode)
{
    auto promiseIt = m_resolvers.find(id);
    if (promiseIt != m_resolvers.end()) {
        (void)promiseIt->second(actionCode);
        m_resolvers.erase(promiseIt);
    }
}
