/*
* Audacity: A Digital Audio Editor
*/
#include "usageinfomodule.h"

#include "framework/global/modularity/ioc.h"

#include "internal/usageinfoservice.h"

using namespace au::usageinfo;

static const std::string mname("usageinfo");

std::string UsageInfoModule::moduleName() const
{
    return mname;
}

void UsageInfoModule::registerExports()
{
    m_usageInfoService = std::make_shared<UsageInfoService>();

    globalIoc()->registerExport<IUsageInfo>(mname, m_usageInfoService);
    globalIoc()->registerExport<muse::update::IUpdateRequestParamsProvider>(mname, m_usageInfoService);
}

void UsageInfoModule::onInit(const muse::IApplication::RunMode&)
{
    m_usageInfoService->init();
}
