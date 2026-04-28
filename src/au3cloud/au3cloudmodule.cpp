/*
* Audacity: A Digital Audio Editor
*/
#include <QQmlEngine>
#include <QtQml>

#include "au3cloudmodule.h"

#include "au3cloud/iau3audiocomservice.h"
#include "au3cloud/iusageinfo.h"
#include "framework/ui/iuiactionsregister.h"

#include "internal/au3cloudconfiguration.h"
#include "internal/au3cloudservice.h"
#include "internal/au3audiocomservice.h"
#include "internal/au3cloudactionscontroller.h"
#include "internal/clouduiactions.h"

#include "view/accountmodel.h"
#include "dev/cloudtestsmodel.h"

using namespace au::au3cloud;

static const std::string mname("au3cloud");

std::string Au3CloudModule::moduleName() const
{
    return mname;
}

void Au3CloudModule::registerExports()
{
    m_cloudConfiguration = std::make_shared<Au3CloudConfiguration>();
    globalIoc()->registerExport<IAu3CloudConfiguration>(mname, m_cloudConfiguration);

    m_cloudService = std::make_shared<Au3CloudService>();
    globalIoc()->registerExport<au3cloud::IAuthorization>(mname, m_cloudService);
    globalIoc()->registerExport<au3cloud::IUsageInfo>(mname, m_cloudService);
}

void Au3CloudModule::onInit(const muse::IApplication::RunMode&)
{
    m_cloudService->init();
    m_cloudConfiguration->init();
}

void Au3CloudModule::onDeinit()
{
}

void Au3CloudModule::registerUiTypes()
{
    qmlRegisterType<CloudTestsModel>("Audacity.Cloud", 1, 0, "CloudTestsModel");
    qmlRegisterType<AccountModel>("Audacity.Cloud", 1, 0, "AccountModel");
}

muse::modularity::IContextSetup* Au3CloudModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new Au3CloudContext(ctx);
}

// =====================================================
// Au3CloudContext
// =====================================================

void Au3CloudContext::registerExports()
{
    m_audioComService = std::make_shared<Au3AudioComService>(iocContext());
    m_actionsController = std::make_shared<Au3CloudActionsController>(iocContext());
    m_uiActions = std::make_shared<CloudUiActions>();

    ioc()->registerExport<au3cloud::IAu3AudioComService>(mname, m_audioComService);
}

void Au3CloudContext::onInit(const muse::IApplication::RunMode&)
{
    m_audioComService->init();
    m_actionsController->init();

    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(mname);
    if (ar) {
        ar->reg(m_uiActions);
    }
}

void Au3CloudContext::onDeinit()
{
    if (m_audioComService) {
        m_audioComService->deinit();
    }
}
