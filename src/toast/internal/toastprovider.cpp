/*
* Audacity: A Digital Audio Editor
*/
#include "toastprovider.h"
#include <memory>

using namespace au::toast;

void ToastProvider::show(std::shared_ptr<ToastItem> item)
{
    if (static_cast<int>(m_toasts.size()) >= m_maxItems) {
        m_toastDismissed.send(m_toasts.front()->id());
        m_toasts.erase(m_toasts.begin());
    }

    m_toasts.emplace_back(item);
    m_toastAdded.send(m_toasts.back());
}

void ToastProvider::setMaxItems(int maxItems)
{
    m_maxItems = maxItems;
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
            return;
        }
    }
}
