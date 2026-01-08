/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/modularity/ioc.h"

#include "internal/toast.h"
#include "internal/toastprovider.h"
#include "view/toastlistmodel.h"

#include "toastmodule.h"

using namespace au::toast;
using namespace muse;
using namespace muse::modularity;

static void toast_init_qrc()
{
    Q_INIT_RESOURCE(toast);
}

std::string ToastModule::moduleName() const
{
    return "toastnotification";
}

void ToastModule::registerExports()
{
    m_toast = std::make_shared<Toast>();
    m_toastProvider = std::make_shared<ToastProvider>();

    ioc()->registerExport<IToast>(moduleName(), m_toast);
    ioc()->registerExport<IToastProvider>(moduleName(), m_toastProvider);
}

void ToastModule::resolveImports()
{
}

void ToastModule::registerResources()
{
    toast_init_qrc();
}

void ToastModule::registerUiTypes()
{
    qmlRegisterType<ToastListModel>("Audacity.Toast", 1, 0, "ToastListModel");
}

void ToastModule::onInit(const IApplication::RunMode&)
{
    m_toast->init();
}

void ToastModule::onDeinit()
{
}
