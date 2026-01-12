/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/modularity/ioc.h"

#include "internal/toastservice.h"
#include "internal/toastprovider.h"
#include "view/toastlistmodel.h"
#include "dev/toasttestsmodel.h"

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
    m_toastService = std::make_shared<ToastService>();
    m_toastProvider = std::make_shared<ToastProvider>();

    ioc()->registerExport<IToastService>(moduleName(), m_toastService);
    ioc()->registerExport<IToastProvider>(moduleName(), m_toastProvider);
}

void ToastModule::registerResources()
{
    toast_init_qrc();
}

void ToastModule::registerUiTypes()
{
    qmlRegisterType<ToastListModel>("Audacity.Toast", 1, 0, "ToastListModel");
    qmlRegisterType<ToastTestsModel>("Audacity.Toast", 1, 0, "ToastTestsModel");
}
