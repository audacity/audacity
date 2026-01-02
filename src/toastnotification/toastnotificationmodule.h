/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <string>

#include "modularity/imodulesetup.h"

namespace au::toastnotification {
class ToastNotificationController;
class ToastNotificationUiActions;
class ToastNotification;
class ToastNotificationModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<ToastNotificationController> m_controller;
    std::shared_ptr<ToastNotificationUiActions> m_uiActions;
    std::shared_ptr<ToastNotification> m_notification;
};
}
