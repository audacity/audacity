/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <vector>
#include <map>

#include "framework/global/async/channel.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/progress.h"
#include "framework/global/async/promise.h"

#include "toast/itoastprovider.h"
#include "toast/toasttypes.h"

namespace au::toast {
class ToastProvider : public IToastProvider, public muse::async::Asyncable
{
public:
    muse::async::Promise<ToastActionCode> show(ToastItem item) override;

    muse::async::Channel <std::shared_ptr<ToastItem> > toastAdded() const override;
    muse::async::Channel<int> toastDismissed() const override;

    void dismissToast(int id) override;
    void executeAction(int id, ToastActionCode actionCode) override;

private:
    void cleanup(int id);
    void checkProgress(int id);
    void checkTimer(int id);
    void resolveToast(int id, ToastActionCode actionCode);

    std::vector<std::shared_ptr<ToastItem> > m_toasts;

    muse::async::Channel<std::shared_ptr<ToastItem> > m_toastAdded;
    muse::async::Channel<int> m_toastDismissed;

    std::map<int, std::unique_ptr<QTimer> > m_progressTimers;
    std::map<int, std::shared_ptr<muse::Progress> > m_progresses;
    std::map<int, muse::async::Promise<ToastActionCode>::Resolve> m_resolvers;
};
}
