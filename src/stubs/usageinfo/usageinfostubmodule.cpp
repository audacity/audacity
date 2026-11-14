/*
* Audacity: A Digital Audio Editor
*/
#include "usageinfostubmodule.h"

#include "usageinfostub.h"

using namespace au::usageinfo;

static const std::string mname("usageinfo");

std::string UsageInfoModule::moduleName() const
{
    return mname;
}

void UsageInfoModule::registerExports()
{
    globalIoc()->registerExport<IUsageInfo>(mname, new UsageInfoStub());
}
