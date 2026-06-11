/*
* Audacity: A Digital Audio Editor
*/
#include "usageinfostub.h"

using namespace au::usageinfo;

void UsageInfoStub::setSendAnonymousUsageInfo(bool)
{
}

bool UsageInfoStub::getSendAnonymousUsageInfo() const
{
    return false;
}

std::string UsageInfoStub::instanceId() const
{
    return std::string();
}

void UsageInfoStub::setUserId(const std::string&)
{
}

muse::async::Notification UsageInfoStub::usageInfoChanged() const
{
    return muse::async::Notification();
}
