/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "framework/global/async/channel.h"
#include "framework/global/async/asyncable.h"

#include "toast/itoastprovider.h"

namespace au::toast {
class ToastProvider : public IToastProvider, public muse::async::Asyncable
{
public:
    void show(std::shared_ptr<ToastItem> item) override;

    void setMaxItems(int maxItems) override;

    muse::async::Channel <std::shared_ptr<ToastItem> > toastAdded() const override;
    muse::async::Channel<int> toastDismissed() const override;

    void dismissToast(int id) override;

private:
    std::vector<std::shared_ptr<ToastItem> > m_toasts;

    muse::async::Channel<std::shared_ptr<ToastItem> > m_toastAdded;
    muse::async::Channel<int> m_toastDismissed;

    int m_maxItems = 0;
};
}
