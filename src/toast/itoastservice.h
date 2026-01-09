/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/ui/view/iconcodes.h"
#include "framework/global/progress.h"
#include "framework/global/async/promise.h"

#include "toasttypes.h"

namespace au::toast {
class IToastService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IToast)
public:
    virtual ~IToastService() = default;
    virtual void init() = 0;
    virtual muse::async::Promise<ToastActionCode> show(const std::string& title, const std::string& message,
                                                       muse::ui::IconCode::Code iconCode = muse::ui::IconCode::Code::NONE,
                                                       bool dismissible = true, const std::vector<ToastAction>& actions = {}) = 0;
    virtual muse::async::Promise<ToastActionCode> showWithTimeout(const std::string& title, const std::string& message,
                                                                  std::chrono::seconds timeout,
                                                                  muse::ui::IconCode::Code iconCode = muse::ui::IconCode::Code::NONE,
                                                                  bool dismissible = true,
                                                                  const std::vector<ToastAction>& actions = {}) = 0;
    virtual void showSuccess(const std::string& title, const std::string& message) = 0;
    virtual void showError(const std::string& title, const std::string& message) = 0;
    virtual void showInfo(const std::string& title, const std::string& message) = 0;
    virtual void showWarning(const std::string& title, const std::string& message) = 0;
    virtual muse::async::Promise<ToastActionCode> showWithProgress(const std::string& title, const std::string& message,
                                                                   std::shared_ptr<muse::Progress> progress,
                                                                   muse::ui::IconCode::Code iconCode = muse::ui::IconCode::Code::NONE,
                                                                   bool dismissible = false,
                                                                   const std::vector<ToastAction>& actions = {}) = 0;
};
}
