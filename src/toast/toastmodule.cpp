/*
* Audacity: A Digital Audio Editor
*/
#include "toastmodule.h"

#include <QQuickItem>

#include "framework/global/modularity/ioc.h"

#include "internal/toastservice.h"
#include "internal/toastprovider.h"
#include "view/toastlistmodel.h"
#include "dev/toasttestsmodel.h"

using namespace au::toast;
using namespace muse;
using namespace muse::modularity;

static const std::string mname("toastnotification");

static void toast_init_qrc()
{
    Q_INIT_RESOURCE(toast);
}

std::string ToastModule::moduleName() const
{
    return mname;
}

void ToastModule::registerExports()
{
    m_toastProvider = std::make_shared<ToastProvider>();
    m_toastService = std::make_shared<ToastService>();

    globalIoc()->registerExport<IToastProvider>(mname, m_toastProvider);
    globalIoc()->registerExport<IToastService>(mname, m_toastService);
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

IContextSetup* ToastModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new ToastContext(ctx);
}

// =====================================================
// ToastContext
// =====================================================

void ToastContext::onDeinit()
{
}
