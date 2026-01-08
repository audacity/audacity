/*
* Audacity: A Digital Audio Editor
*/
#include "framework/ui/view/iconcodes.h"

#include "toast.h"

using namespace muse::ui;
using namespace au::toast;

namespace {
constexpr int DEFAULT_MAX_ITEMS = 5;
}

void Toast::init()
{
    toastProvider()->setMaxItems(DEFAULT_MAX_ITEMS);
}

void Toast::show(const std::string& title, const std::string& message, muse::ui::IconCode::Code iconCode, bool dismissible,
                 const std::vector<ToastAction>& actions)
{
    const auto item = std::make_shared<ToastItem>(title, message, iconCode, dismissible, std::chrono::seconds(0), actions);
    toastProvider()->show(item);
}

void Toast::showWithTimeout(const std::string& title, const std::string& message, std::chrono::seconds timeout,
                            muse::ui::IconCode::Code iconCode, bool dismissible, const std::vector<ToastAction>& actions)
{
    const auto item = std::make_shared<ToastItem>(title, message, iconCode, dismissible, timeout, actions);
    toastProvider()->show(item);
}

void Toast::showSucess(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::TICK, true, {});
}

void Toast::showError(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::ERROR, true, {});
}

void Toast::showInfo(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::INFO, true, {});
}

void Toast::showWarning(const std::string& title, const std::string& message)
{
    show(title, message, IconCode::Code::WARNING, true, {});
}

void Toast::showWithProgress(const std::string& title, const std::string& message, std::shared_ptr<muse::Progress> progress,
                             muse::ui::IconCode::Code iconCode, bool dismissible, const std::vector<ToastAction>& actions)
{
    const auto item = std::make_shared<ToastItem>(title, message, iconCode, dismissible, actions, progress);
}
