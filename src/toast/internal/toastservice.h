/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>

#include "framework/global/modularity/ioc.h"
#include "framework/global/progress.h"
#include "toast/itoastprovider.h"

#include "toast/itoastservice.h"

namespace au::toast {
class ToastService : public IToastService, public muse::Injectable
{
    muse::Inject<IToastProvider> toastProvider;

public:
    void init() override;
    muse::async::Promise<ToastActionCode> show(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode,
                                               bool dismissible, const std::vector<ToastAction>& actions) override;
    muse::async::Promise<ToastActionCode> showWithTimeout(const std::string& title, const std::string& message,
                                                          std::chrono::seconds timeout, muse::ui::IconCode::Code iconCode, bool dismissible,
                                                          const std::vector<ToastAction>& actions) override;
    void showSuccess(const std::string& title, const std::string& message) override;
    void showError(const std::string& title, const std::string& message) override;
    void showInfo(const std::string& title, const std::string& message) override;
    void showWarning(const std::string& title, const std::string& message) override;
    muse::async::Promise<ToastActionCode> showWithProgress(const std::string& title, const std::string& message,
                                                           std::shared_ptr<muse::Progress> progress, muse::ui::IconCode::Code iconCode,
                                                           bool dismissible, const std::vector<ToastAction>& actions) override;
};
}
