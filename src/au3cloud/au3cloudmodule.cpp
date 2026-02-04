/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/modularity/ioc.h"

#include "au3cloudmodule.h"
#include "dev/cloudtestsmodel.h"

using namespace au::au3cloud;

std::string Au3CloudModule::moduleName() const
{
    return "au3cloud";
}

void Au3CloudModule::registerExports()
{
    m_cloudService = std::make_shared<Au3CloudService>(iocContext());
    ioc()->registerExport<IAuthorization>(moduleName(), m_cloudService);
    ioc()->registerExport<IUsageInfo>(moduleName(), m_cloudService);
}

void Au3CloudModule::onInit(const muse::IApplication::RunMode&)
{
    m_cloudService->init();
}

void Au3CloudModule::registerUiTypes()
{
    qmlRegisterType<CloudTestsModel>("Audacity.Cloud", 1, 0, "CloudTestsModel");
}
