/*
* Audacity: A Digital Audio Editor
*/
#include "au3cloudstubmodule.h"

#include "au3audiocomservicestub.h"
#include "au3cloudconfigurationstub.h"
#include "authorizationstub.h"
#include "usageinfostub.h"

using namespace au::au3cloud;

static const std::string mname("au3cloud");

std::string Au3CloudModule::moduleName() const
{
    return mname;
}

void Au3CloudModule::registerExports()
{
    globalIoc()->registerExport<IAu3CloudConfiguration>(mname, new Au3CloudConfigurationStub());
    globalIoc()->registerExport<IAuthorization>(mname, new AuthorizationStub());
    globalIoc()->registerExport<IUsageInfo>(mname, new UsageInfoStub());
}

muse::modularity::IContextSetup* Au3CloudModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new Au3CloudStubContext(ctx);
}

// =====================================================
// Au3CloudStubContext
// =====================================================

void Au3CloudStubContext::registerExports()
{
    ioc()->registerExport<IAu3AudioComService>(mname, new Au3AudioComServiceStub(iocContext()));
}
