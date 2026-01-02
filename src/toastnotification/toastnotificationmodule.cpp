/*
* Audacity: A Digital Audio Editor
*/
#include <memory>
#include <string>

#include "framework/global/modularity/ioc.h"
#include "framework/ui/iuiactionsregister.h"

#include "toastnotification.h"
#include "internal/toastnotificationcontroller.h"
#include "internal/toastnotificationuiactions.h"
#include "view/toastnotificationlistmodel.h"

#include "toastnotificationmodule.h"

using namespace au::toastnotification;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

static void toastnotification_init_qrc()
{
    Q_INIT_RESOURCE(toastnotification);
}

std::string ToastNotificationModule::moduleName() const
{
    return "toastnotification";
}

void ToastNotificationModule::registerExports()
{
    m_controller = std::make_shared<ToastNotificationController>();
    m_uiActions = std::make_shared<ToastNotificationUiActions>();

    m_notification = std::make_shared<ToastNotification>();
    ioc()->registerExport<IToastNotification>(moduleName(), m_notification);
}

void ToastNotificationModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
}

void ToastNotificationModule::registerResources()
{
    toastnotification_init_qrc();
}

void ToastNotificationModule::registerUiTypes()
{
    qmlRegisterType<ToastNotificationListModel>("Audacity.ToastNotification", 1, 0, "ToastNotificationListModel");
}

void ToastNotificationModule::onInit(const IApplication::RunMode&)
{
    m_controller->init();
}

void ToastNotificationModule::onDeinit()
{
}
