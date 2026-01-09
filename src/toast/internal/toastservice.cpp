/*
* Audacity: A Digital Audio Editor
*/
#include "framework/ui/view/iconcodes.h"
#include "toast/internal/toastitem.h"

#include "toastservice.h"

using namespace muse::ui;
using namespace au::toast;

namespace {
constexpr int DEFAULT_MAX_ITEMS = 5;
}

void ToastService::init()
{
    toastProvider()->setMaxItems(DEFAULT_MAX_ITEMS);
}

void ToastService::show(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
                        const std::vector<ToastAction>& actions)
{
    toastProvider()->show(ToastItem(title, message, iconCode, dismissible, std::chrono::seconds(0), actions));
}

void ToastService::showWithTimeout(const std::string& title, const std::string& message, std::chrono::seconds timeout,
                                   muse::ui::IconCode::Code iconCode, bool dismissible, const std::vector<ToastAction>& actions)
{
    toastProvider()->show(ToastItem(title, message, iconCode, dismissible, timeout, actions));
}

void ToastService::showSuccess(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::TICK, true, {});
}

void ToastService::showError(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::ERROR, true, {});
}

void ToastService::showInfo(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::INFO, true, {});
}

void ToastService::showWarning(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::WARNING, true, {});
}

void ToastService::showWithProgress(const std::string& title, const std::string& message, std::shared_ptr<muse::Progress> progress,
                                    muse::ui::IconCode::Code iconCode, bool dismissible, const std::vector<ToastAction>& actions)
{
    toastProvider()->show(ToastItem(title, message, iconCode, dismissible, actions, progress));
}
