/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "usageinfo/iusageinfo.h"

namespace au::usageinfo {
class UsageInfoStub : public IUsageInfo
{
public:
    void setSendAnonymousUsageInfo(bool allow) override;
    bool getSendAnonymousUsageInfo() const override;

    std::string instanceId() const override;

    void setUserId(const std::string& userId) override;

    muse::async::Notification usageInfoChanged() const override;
};
}
